{-# LANGUAGE RecordWildCards #-}

module Maelstrom.Node (
    spawnNode,
    ReqHandler,
    Node,
    waitNode,
    simpleHandler,
) where

import Control.Concurrent (newMVar, putMVar, takeMVar)
import Control.Concurrent.Async (Async, async, link, wait)
import Control.Monad (forever)
import Data.Aeson (
    FromJSON,
    ToJSON,
    Value,
    eitherDecode,
    eitherDecode',
    (.:),
 )
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as AKM
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as AT
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as C8
import Data.Foldable.WithIndex (FoldableWithIndex (ifoldr'))
import Data.Text (Text)
import Data.Void (Void)
import Maelstrom.IO qualified as MIO
import Maelstrom.Message (
    InitRequest,
    InitResponse,
    Message (Message),
    MessageId,
    getNodeId,
    mkInitResponse,
 )

data Node = Node
    { nodeId :: Text
    , mainLoopAsync :: Async Void
    }

data ReqHandler req resp
    = Simple (req -> IO resp)
    | Raw (C8.ByteString -> IO C8.ByteString)

simpleHandler :: (req -> IO resp) -> ReqHandler req resp
simpleHandler = Simple

spawnNode :: (FromJSON req, ToJSON resp) => ReqHandler req resp -> IO Node
spawnNode hlr = do
    initReq <- awaitInitMessage
    let msgIdSeed = 1
        nodeId = getNodeId initReq

    getNextId <- mkIdGen msgIdSeed
    initMsgId <- getNextId
    respondInitOk (mkInitResponse initReq initMsgId)
    mainLoopAsync <- spawnMainLoop hlr getNextId -- TODO: async process, add Async to node, add node kill
    link mainLoopAsync
    pure $ Node{..}

-- | Will wait node main loop to finish effectively blocking current thread forever
waitNode :: Node -> IO Void
waitNode (Node _ as) = wait as

mkIdGen :: Integer -> IO (IO MessageId)
mkIdGen seed = do
    mvi <- newMVar seed
    pure $ do
        ci <- takeMVar mvi
        putMVar mvi (succ ci)
        pure ci

awaitInitMessage :: IO (Message InitRequest)
awaitInitMessage = do
    m <- MIO.receiveMessage
    case eitherDecode m of
        Right m' -> pure m'
        Left err ->
            error . mconcat $
                [ "Could not parse init message from:\n" <> C8.unpack m
                , "\nError:\n" <> err
                ]

respondInitOk :: Message InitResponse -> IO ()
respondInitOk m = do
    MIO.sendEncoded m

spawnMainLoop ::
    forall req resp.
    (FromJSON req, ToJSON resp) =>
    ReqHandler req resp ->
    IO MessageId ->
    IO (Async Void)
spawnMainLoop msgHandler getNextId =
    async . forever $ do
        rawRequest <- MIO.receiveMessage
        case msgHandler of
            Simple h -> getNextId >>= handleSimple rawRequest h
            Raw _h -> error "TODO: Raw handler"

handleSimple ::
    forall req resp.
    (FromJSON req, ToJSON resp) =>
    C8.ByteString ->
    (req -> IO resp) ->
    Integer ->
    IO ()
handleSimple rawMsg handle respId =
    case eitherDecode' rawMsg :: Either String (Message Aeson.Object) of
        Left e -> error e
        Right (Message from to body) -> do
            (commonResponsePart, usersJSON) <- parseBody body

            userResp <- handle usersJSON

            case Aeson.toJSON userResp of
                Aeson.Object userResponse ->
                    MIO.sendEncoded $ Message to from (commonResponsePart <> userResponse)
                other -> error $ "Bad user response: " <> show other
  where
    parseBody body = do
        let (commonPart, userPart) = splitBody body
        parsedCommonPart <-
            maybe
                (failWith $ "Can't get message id from incoming message body: " <> show body)
                pure
                (toCommonResp respId commonPart)

        usersJSON <- case Aeson.fromJSON @req (Aeson.toJSON userPart) of
            AT.Error s -> error $ "users re-jsoning failed: " <> s
            AT.Success a -> pure a

        pure (parsedCommonPart, usersJSON)

    failWith msg = error $ "Error: Simple handler: " <> msg

toCommonResp :: Integer -> Aeson.Object -> Maybe (KM.KeyMap Value)
toCommonResp msgId obj = do
    incId :: Integer <- Aeson.parseMaybe (.: "msg_id") obj
    pure $
        KM.fromList
            [ ("msg_id", Aeson.toJSON msgId)
            , ("in_reply_to", Aeson.toJSON incId)
            ]

splitBody :: Aeson.Object -> (Aeson.Object, Aeson.Object)
splitBody = ifoldr' f (mempty, mempty) -- TODO: toJSON both values in pair?
  where
    -- :: Key -> Value -> (Object, Object) -> (Object, Object)
    f k v (common, user) = case k of
        "msg_id" -> (AKM.singleton k v <> common, user)
        _ -> (common, AKM.singleton k v <> user)
