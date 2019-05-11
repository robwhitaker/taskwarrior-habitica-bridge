{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web where

import           Control.Exception    (catch)

import           Data.Aeson           (parseJSON)
import           Data.Aeson.Types     (FromJSON, Value (Null), parseEither)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           Data.String          (fromString)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.UUID            as UUID

import           Network.HTTP.Req     (Option, Req, ReqBodyJson (..),
                                       Scheme (Https), Url, (/:), (=:))
import qualified Network.HTTP.Req     as Req

import           Types

{- ENDPOINT DEFINITIONS  -}
habiticaApiV3 :: Url 'Https
habiticaApiV3 = Req.https "habitica.com" /: "api" /: "v3"

tasksEndpoint :: Url 'Https
tasksEndpoint = habiticaApiV3 /: "tasks" /: "user"

taskEndpoint :: UUID -> Url 'Https
taskEndpoint (UUID taskId) = habiticaApiV3 /: "tasks" /: UUID.toText taskId

scoreEndpoint :: UUID -> ScoreDirection -> Url 'Https
scoreEndpoint (UUID taskId) dir =
    habiticaApiV3 /: "tasks" /: UUID.toText taskId /: "score" /:
    (T.toLower $ T.pack $ show dir)

{- HELPER FUNCTIONS -}
newtype HabiticaHeaders =
    HabiticaHeaders (Option 'Https)

habiticaHeaders :: Text -> Text -> Maybe HabiticaHeaders
habiticaHeaders textUUID textApiKey =
    (\uuid apiKey ->
         HabiticaHeaders $
         (Req.header "x-api-user" $ B.toStrict $ fromString $ UUID.toString $ uuid) <>
         (Req.header "x-api-key" $ B.toStrict $ fromString $ UUID.toString apiKey)) <$>
    UUID.fromText textUUID <*>
    UUID.fromText textApiKey

{- RUNNING REQUESTS -}
runHabiticaReq ::
       FromJSON a => Req (Either String (HabiticaResponse a)) -> IO (HabiticaResponse a)
runHabiticaReq req = do
    catch (either ParseError id <$> Req.runReq reqConf req) $ return . HttpException
    -- Throw errors out the window for now
    -- TODO: Actually handle them properly
  where
    reqConf = Req.defaultHttpConfig {Req.httpConfigCheckResponse = \_ _ _ -> Nothing}

-- TODO: Temp runner just to get things working
runHabiticaReq' :: Req Value -> IO ()
runHabiticaReq' req = do
    catch (Req.runReq reqConf req) (\(_ :: Req.HttpException) -> return Null) >>
        return ()
    -- Throw errors out the window for now
    -- TODO: Actually handle them properly
  where
    reqConf = Req.defaultHttpConfig {Req.httpConfigCheckResponse = \_ _ _ -> Nothing}

{- HABITICA REQUESTS -}
habiticaCreateOrUpdateRequest ::
       HabiticaHeaders
    -> HabiticaTask
    -> Req (Either String (HabiticaResponse HabiticaTask))
habiticaCreateOrUpdateRequest (HabiticaHeaders headers) habiticaTask@(HabiticaTask task) =
    let mkReq =
            case taskHabiticaId task of
                Just id -> Req.req Req.PUT (taskEndpoint id)
                Nothing -> Req.req Req.POST tasksEndpoint
     in mkReq (ReqBodyJson habiticaTask) Req.jsonResponse headers >>=
        return . parseEither parseJSON . Req.responseBody

habiticaGetTasks ::
       HabiticaHeaders
    -> Maybe Text
    -> Req (Either String (HabiticaResponse [HabiticaTask]))
habiticaGetTasks (HabiticaHeaders headers) maybeQueryParam =
    Req.req
        Req.GET
        tasksEndpoint
        Req.NoReqBody
        Req.jsonResponse
        (case maybeQueryParam of
             Nothing -> headers
             _       -> (headers <> Req.queryParam "type" maybeQueryParam)) >>=
    return . parseEither parseJSON . Req.responseBody

habiticaGetTask ::
       HabiticaHeaders -> UUID -> Req (Either String (HabiticaResponse HabiticaTask))
habiticaGetTask (HabiticaHeaders headers) uuid =
    Req.req Req.GET (taskEndpoint uuid) Req.NoReqBody Req.jsonResponse headers >>=
    return . parseEither parseJSON . Req.responseBody

-- TODO: Better return value?
habiticaDeleteTask :: HabiticaHeaders -> UUID -> Req Value
habiticaDeleteTask (HabiticaHeaders headers) uuid =
    Req.req Req.DELETE (taskEndpoint uuid) Req.NoReqBody Req.jsonResponse headers >>=
    return . Req.responseBody

data ScoreDirection
    = Up
    | Down
    deriving (Show, Eq)

-- TODO: Better return value?
habiticaScoreTask :: HabiticaHeaders -> UUID -> ScoreDirection -> Req Value
habiticaScoreTask (HabiticaHeaders headers) uuid dir =
    Req.req Req.POST (scoreEndpoint uuid dir) Req.NoReqBody Req.jsonResponse headers >>=
    return . Req.responseBody
