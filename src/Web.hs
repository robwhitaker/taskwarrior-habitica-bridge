{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Web where

import           Control.Concurrent   (threadDelay)
import           Control.Exception    (catch)
import           Control.Monad        (void)

import           Data.Aeson           (parseJSON)
import           Data.Aeson.Types     (FromJSON, Value (Null), parseEither)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           Data.String          (fromString)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.UUID            as UUID

import           Network.HTTP.Req     (HttpResponse (..), Option, Req,
                                       ReqBodyJson (..), Scheme (Https), Url,
                                       (/:), (=:))
import qualified Network.HTTP.Req     as Req

import           Types

{- ENDPOINT DEFINITIONS  -}
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

{- HELPER FUNCTIONS -}
newtype HabiticaHeaders =
    HabiticaHeaders (Option 'Https)

habiticaHeaders :: Text -> Text -> Maybe HabiticaHeaders
habiticaHeaders textUUID textApiKey =
    mkHeaders <$> UUID.fromText textUUID <*> UUID.fromText textApiKey
  where
    mkHeaders uuid apiKey =
        HabiticaHeaders $ mconcat
            [ Req.header "x-api-user" (B.toStrict $ fromString $ UUID.toString uuid)
            , Req.header "x-api-key" (B.toStrict $ fromString $ UUID.toString apiKey)
            ]

-- Type signature for this is weird, so let GHC figure it out
toHabiticaRequest f req = do
    r <- parseEither parseJSON . Req.responseBody <$> req
    return (fmap (fmap f) r)

ignoreValue :: Value -> ()
ignoreValue = const ()

{- TYPE ALIASES -}

type HabiticaRequest a = Req (Either String (HabiticaResponse a))

{- RUNNING REQUESTS -}
runHabiticaReq :: FromJSON a => HabiticaRequest a -> IO (HabiticaResponse a)
runHabiticaReq req = do
    -- Add a pause before Habitica requests so the server doesn't choke when more than one
    -- request happens
    threadDelay 1000000
    -- Do some simple error handling to prevent the user's API key from getting spit into
    -- the terminal on a failure
    catch (either ParseError id <$> Req.runReq reqConf req) $ return . HttpException
  where
    reqConf = Req.defaultHttpConfig {Req.httpConfigCheckResponse = \_ _ _ -> Nothing}

{- HABITICA REQUESTS -}
habiticaCreateOrUpdateRequest :: HabiticaHeaders -> HabiticaTask -> HabiticaRequest HabiticaTask
habiticaCreateOrUpdateRequest (HabiticaHeaders headers) habiticaTask@(HabiticaTask task) =
    toHabiticaRequest id $
        mkReq (ReqBodyJson habiticaTask) Req.jsonResponse headers
  where
    mkReq =
        case taskHabiticaId task of
            Just id -> Req.req Req.PUT (taskEndpoint id)
            Nothing -> Req.req Req.POST tasksEndpoint

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
