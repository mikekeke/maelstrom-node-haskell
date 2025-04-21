{-# LANGUAGE TemplateHaskell #-}

module Maelstrom.Node (
    spawnNode,
    ReqHandler,
    debug,
    Node,
) where

import Control.Concurrent (newMVar, putMVar, takeMVar)
import Control.Monad (forever, guard)
import Data.Aeson (
    FromJSON,
    ToJSON,
    Value,
    eitherDecode,
    eitherDecode',
    encode,
    (.:),
 )
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as AKM
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as AT
import Data.Aeson.Types qualified as Aeson
import Data.Foldable.WithIndex (FoldableWithIndex (ifoldr'))
import Data.Text (Text)
import Data.Void (Void)
import Maelstrom.IO qualified as MIO
import Maelstrom.Message (
    CommonMsg,
    InitRequest,
    InitResponse,
    Message (Message),
    MessageId,
    getInitId,
    mkInitResponse,
    msgToString,
 )

data Node = Node
    { nodeId :: Text
    }

type ReqHandler req resp = req -> resp

spawnNode :: (FromJSON req, ToJSON resp) => ReqHandler req resp -> IO Node
spawnNode hlr = do
    initReq <- awaitInitMessage
    let msgIdSeed = 1
    getNextId <- mkIdGen msgIdSeed
    initMsgId <- getNextId
    guard (initMsgId == msgIdSeed)
    respondInitOk (mkInitResponse initReq msgIdSeed)
    let node = Node (getInitId initReq)
    spawnMainLoop hlr getNextId -- TODO: async process, add Async to node, add node kill
    pure node

mkIdGen :: Integer -> IO (IO MessageId)
mkIdGen seed = do
    -- i <- newMVar# seed
    mvi <- newMVar seed
    pure $ do
        ci <- takeMVar mvi
        putMVar mvi (succ ci)
        pure ci

awaitInitMessage :: IO (Message InitRequest)
awaitInitMessage = do
    m <- MIO.receiveMessage

    case eitherDecode m of
        Left _err -> error "Parse err" -- <> LBS.pack  err
        Right m' -> do
            pure m'

respondInitOk :: Message InitResponse -> IO ()
respondInitOk m = do
    MIO.sendMessage (encode m)

spawnMainLoop ::
    forall req resp.
    (FromJSON req, ToJSON resp) =>
    ReqHandler req resp ->
    IO MessageId ->
    IO Void
spawnMainLoop msgHandler getNextId =
    -- TODO: each message handled async?
    forever $ do
        rawRequest <- MIO.receiveMessage

        MIO.logNB $ "## Raw request: " <> rawRequest

        case eitherDecode' rawRequest :: Either String (Message Aeson.Object) of
            Left e -> error e
            Right (Message from to b) -> do
                msgId <- getNextId
                MIO.logN $ "Parsed req body: " <> show b
                let (common, users) = splitBody b
                    Just commonResp = toCommonResp msgId common -- TODO: handle error (should not happen)

                -- _ = AT.parseEither (a -> Parser b) a
                users' <- case Aeson.fromJSON @req (Aeson.toJSON users) of
                    AT.Error s -> error $ "users re-jsoning failed: " <> s
                    AT.Success a -> pure a

                let userResp = msgHandler users'

                respToSend <- case Aeson.toJSON userResp of
                    Aeson.Object o -> pure (commonResp <> o)
                    other -> error $ "Bad user response: " <> show other
                MIO.logN $ "respToSend: " <> show respToSend
                MIO.logN $ "respToSend enc: " <> show (encode respToSend)
                MIO.sendMessage (encode $ Message to from respToSend)
  where

-- toCommonResp :: AT.Object -> Integer
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

-- $(deriveJSON defaultOptions{fieldLabelModifier = snakeCase} ''CommonMsg)

-- $(deriveJSON defaultOptions ''CommonMsg)

-- >>> debug
-- "Success (CommonMsg {cmMsgId = 1})"
debug :: IO String
debug = do
    (Message _ _ body) <-
        Aeson.eitherDecodeFileStrict' @(Message Aeson.Object) "req.json"
            >>= either error pure
    let (common, users) = splitBody body
    pure $ show $ Aeson.fromJSON @CommonMsg (Aeson.toJSON common)
