{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Web
    (
    -- * Types
      HabiticaHeaders
    , HabiticaRequest
    , ScoreDirection(..)

    -- * Create headers for requests
    , habiticaHeaders

    -- * Creating requests
    , habiticaCreateOrUpdateRequest
    , habiticaDeleteTask
    , habiticaGetTask
    , habiticaGetTasks
    , habiticaGetUserStats
    , habiticaScoreTask

    -- * Running the requests
    , runHabiticaReq
    ) where

import qualified Control.Concurrent   as Concurrent
import           Control.Exception    (catch)

import qualified Data.Aeson           as Aeson
import           Data.Aeson.Types     (FromJSON, Value)
import qualified Data.Aeson.Types     as Aeson

import qualified Data.ByteString.Lazy as B
import qualified Data.String          as String
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.UUID            as UUID

import           Network.HTTP.Client  (HttpException (..), requestHeaders)
import           Network.HTTP.Req     (HttpException (..), Option, Req,
                                       ReqBodyJson (..), Scheme (Https), Url,
                                       (/:))
import qualified Network.HTTP.Req     as Req

import           Types

-- Endpoint definitions

habiticaApiV3 :: Url 'Https
habiticaApiV3 = Req.https "habitica.com" /: "api" /: "v3"

habiticaApiV4 :: Url 'Https
habiticaApiV4 = Req.https "habitica.com" /: "api" /: "v4"

userEndpoint :: Url 'Https
userEndpoint = habiticaApiV4 /: "user"

tasksEndpoint :: Url 'Https
tasksEndpoint = habiticaApiV3 /: "tasks" /: "user"

taskEndpoint :: UUID -> Url 'Https
taskEndpoint (UUID taskId) = habiticaApiV3 /: "tasks" /: UUID.toText taskId

scoreEndpoint :: UUID -> ScoreDirection -> Url 'Https
scoreEndpoint (UUID taskId) dir =
    habiticaApiV3 /: "tasks" /: UUID.toText taskId /: "score" /: T.toLower (T.pack $ show dir)

-- Helper functions

newtype HabiticaHeaders =
    HabiticaHeaders (Option 'Https)

habiticaHeaders :: Text -> Text -> Maybe HabiticaHeaders
habiticaHeaders textUUID textApiKey =
    mkHeaders <$> UUID.fromText textUUID <*> UUID.fromText textApiKey
  where
    mkHeaders uuid apiKey =
        HabiticaHeaders $ mconcat
            [ Req.header "x-api-user" (B.toStrict $ String.fromString $ UUID.toString uuid)
            , Req.header "x-api-key" (B.toStrict $ String.fromString $ UUID.toString apiKey)
            ]

-- Type signature for this is weird, so let GHC figure it out
toHabiticaRequest f req = do
    r <- Aeson.parseEither Aeson.parseJSON . Req.responseBody <$> req
    return (fmap (fmap f) r)

ignoreValue :: Value -> ()
ignoreValue = const ()

-- Type aliases

type HabiticaRequest a = Req (Either String (HabiticaResponse a))

-- Running requests

runHabiticaReq :: FromJSON a => HabiticaRequest a -> IO (HabiticaResponse a)
runHabiticaReq req = do
    -- Add a pause before Habitica requests so the server doesn't choke when more than one
    -- request happens
    Concurrent.threadDelay 1000000
    -- Do some error handling to prevent the user's API key from getting spit into
    -- the terminal on a failure
    catch (either ParseError id <$> Req.runReq reqConf req) $ return . \case
        VanillaHttpException (HttpExceptionRequest request err) ->
            let
                requestApiMasked = request {
                    requestHeaders = fmap (\header@(headerName, _) ->
                        if headerName == "x-api-key"
                            then (headerName, "(hidden)")
                            else header
                    ) (requestHeaders request)
                }
            in
            HttpException (VanillaHttpException (HttpExceptionRequest requestApiMasked err))

        otherVanilla@(VanillaHttpException _) ->
            HttpException otherVanilla

        JsonHttpException str -> ParseError str
  where
    reqConf = Req.defaultHttpConfig {Req.httpConfigCheckResponse = \_ _ _ -> Nothing}

-- Habitica requests

habiticaCreateOrUpdateRequest :: HabiticaHeaders -> HabiticaTask -> HabiticaRequest HabiticaTask
habiticaCreateOrUpdateRequest (HabiticaHeaders headers) habiticaTask@(HabiticaTask task) =
    toHabiticaRequest id $
        mkReq (ReqBodyJson habiticaTask) Req.jsonResponse headers
  where
    mkReq =
        case taskHabiticaId task of
            Just hId -> Req.req Req.PUT (taskEndpoint hId)
            Nothing  -> Req.req Req.POST tasksEndpoint

habiticaGetTasks :: HabiticaHeaders -> Maybe Text -> HabiticaRequest [HabiticaTask]
habiticaGetTasks (HabiticaHeaders headers) maybeQueryParam =
    toHabiticaRequest id $
        Req.req Req.GET tasksEndpoint Req.NoReqBody Req.jsonResponse
            (maybe headers ((headers <>) . Req.queryParam "type" . Just) maybeQueryParam)

habiticaGetTask :: HabiticaHeaders -> UUID -> HabiticaRequest HabiticaTask
habiticaGetTask (HabiticaHeaders headers) uuid =
    toHabiticaRequest id $
        Req.req Req.GET (taskEndpoint uuid) Req.NoReqBody Req.jsonResponse headers


habiticaDeleteTask :: HabiticaHeaders -> UUID -> HabiticaRequest ()
habiticaDeleteTask (HabiticaHeaders headers) uuid =
    toHabiticaRequest ignoreValue $
        Req.req Req.DELETE (taskEndpoint uuid) Req.NoReqBody Req.jsonResponse headers

data ScoreDirection
    = Up
    | Down
  deriving (Show, Eq)

habiticaScoreTask :: HabiticaHeaders -> UUID -> ScoreDirection -> HabiticaRequest ()
habiticaScoreTask (HabiticaHeaders headers) uuid dir =
    toHabiticaRequest ignoreValue $
        Req.req Req.POST (scoreEndpoint uuid dir) Req.NoReqBody Req.jsonResponse headers

habiticaGetUserStats :: HabiticaHeaders -> HabiticaRequest ()
habiticaGetUserStats (HabiticaHeaders headers) =
    toHabiticaRequest ignoreValue $
        Req.req Req.GET userEndpoint Req.NoReqBody Req.jsonResponse headers
