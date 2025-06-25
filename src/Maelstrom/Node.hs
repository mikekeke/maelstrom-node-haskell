{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Maelstrom.Node (
    spawnNode,
    ReqHandler,
    Node,
    waitNode,
    simpleHandler,
    rawHandler,
    reply,
    messageHandler,
) where

import Control.Concurrent (newMVar, putMVar, takeMVar)
import Control.Concurrent.Async (Async, async, link, wait)
import Control.Monad (forever)
import Data.Aeson (
    FromJSON,
    Key,
    Object,
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
import Data.Functor (void)
import Data.Text (Text)
import Data.Void (Void)
import Maelstrom.IO (logN)
import Maelstrom.IO qualified as MIO
import Maelstrom.Message (
    InitRequest,
    InitResponse,
    Message (Message),
    MessageId,
    getNodeId,
    mkInitResponse,
 )

-- TODO: fix all usages of `error`

data Node = Node
    { nodeId :: Text
    , mainLoopAsync :: Async Void
    }

data ReqHandler where
    RawH ::
        (C8.ByteString -> MessageId -> IO C8.ByteString) ->
        ReqHandler
    SimpleH ::
        (FromJSON req, ToJSON resp) =>
        (req -> IO resp) ->
        ReqHandler
    MessageH ::
        (FromJSON reqBody, ToJSON respBody) =>
        (Message reqBody -> MessageId -> IO (Message respBody)) ->
        ReqHandler

simpleHandler :: (FromJSON req, ToJSON resp) => (req -> IO resp) -> ReqHandler
simpleHandler = SimpleH

messageHandler ::
    (FromJSON reqBody, ToJSON respBody) =>
    (Message reqBody -> MessageId -> IO (Message respBody)) ->
    ReqHandler
messageHandler = MessageH

rawHandler :: (C8.ByteString -> MessageId -> IO C8.ByteString) -> ReqHandler
rawHandler = RawH

spawnNode :: ReqHandler -> IO Node
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

spawnMainLoop ::
    ReqHandler ->
    IO MessageId ->
    IO (Async Void)
spawnMainLoop msgHandler getNextId =
    async . forever $ do
        rawRequest <- MIO.receiveMessage
        void $
            async $ do
                logN "@@ Running in async"
                case msgHandler of
                    SimpleH h -> getNextId >>= handleSimple rawRequest h
                    MessageH h -> getNextId >>= handleMessage rawRequest h
                    RawH h -> do
                        nextId <- getNextId
                        response <- h rawRequest nextId
                        MIO.sendMessage response

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

-- | Gives access to internals of `Message` type: `src`, `dest` and `body` fields
handleMessage ::
    forall req resp.
    (FromJSON req, ToJSON resp) =>
    C8.ByteString ->
    (Message req -> MessageId -> IO (Message resp)) ->
    Integer ->
    IO ()
handleMessage rawMsg handle respId = do
    case eitherDecode' rawMsg :: Either String (Message req) of -- TODO: throw proper exception or log and ignore malformed request?
        Left e -> error e
        Right msg -> do
            response <- handle msg respId
            MIO.sendEncoded response

{- | Utility function to help to reply to `Message`.
     Can be handy for `handleMessage` handler.

    - Request body will be decoded as `Aeson.Object`
    - Response body will be encoded as `Aeson.Object`
    - `msg_id` will be set from `MessageId` argument
    - `in_reply_to` will be handled automatically
    - `src` and `dest` fields of response will be handled automatically

    __Note__: Will crash the node with an error if `msg_id` field is not found in the incoming message, or parsing failed
-}
reply ::
    ToJSON v =>
    Message Object ->
    MessageId ->
    [(Key, v)] ->
    Message Object
reply (Message from to body) replyId resp =
    let commonPart = fst $ splitBody body
        commonResp = toCommonRespUnsafe replyId commonPart
        userProvidedData = Aeson.toJSON <$> KM.fromList resp
     in Message to from (commonResp <> userProvidedData)

handleSimple ::
    forall req resp.
    (FromJSON req, ToJSON resp) =>
    C8.ByteString ->
    (req -> IO resp) ->
    Integer ->
    IO ()
handleSimple rawMsg handle respId =
    case eitherDecode' rawMsg :: Either String (Message Aeson.Object) of
        Left e -> error e -- TODO: throw proper exception or log and ignore malformed request?
        Right (Message from to body) -> do
            (commonResponsePart, usersJSON) <- parseBody body
            userResp <- handle usersJSON
            case Aeson.toJSON userResp of
                Aeson.Object userResponse ->
                    MIO.sendEncoded $ Message to from (commonResponsePart <> userResponse)
                other -> error $ "Bad user response: " <> show other -- TODO: throw proper exception or log and ignore malformed request?
  where
    parseBody body = do
        let (commonPart, userPart) = splitBody body
        parsedCommonPart <-
            maybe
                (failWith $ "Can't get message id from incoming message body: " <> show body)
                pure
                (toCommonResp respId commonPart)

        usersJSON <- case Aeson.fromJSON (Aeson.toJSON userPart) of
            AT.Error s -> error $ "users re-jsoning failed: " <> s
            AT.Success a -> pure a

        pure (parsedCommonPart, usersJSON)

    -- TODO: throw proper exception or log and ignore malformed request?
    failWith msg = error $ "Error: Simple handler: " <> msg

toCommonResp :: Integer -> Object -> Maybe (KM.KeyMap Value)
toCommonResp msgId obj = do
    incId :: Integer <- Aeson.parseMaybe (.: "msg_id") obj
    pure $
        KM.fromList
            [ ("msg_id", Aeson.toJSON msgId)
            , ("in_reply_to", Aeson.toJSON incId)
            ]

toCommonRespUnsafe :: Integer -> Object -> KM.KeyMap Value
toCommonRespUnsafe msgId obj =
    either failBadly id $ do
        incId :: Integer <- Aeson.parseEither (.: "msg_id") obj
        pure $
            KM.fromList
                [ ("msg_id", Aeson.toJSON msgId)
                , ("in_reply_to", Aeson.toJSON incId)
                ]
  where
    failBadly e = error $ "Critical failure while parsing incoming message: " <> e

splitBody :: Object -> (Object, Object)
splitBody = ifoldr' f (mempty, mempty) -- TODO: toJSON both values in pair?
  where
    f :: Key -> Value -> (Object, Object) -> (Object, Object)
    f k v (common, user) = case k of
        "msg_id" -> (AKM.singleton k v <> common, user)
        _ -> (common, AKM.singleton k v <> user)
