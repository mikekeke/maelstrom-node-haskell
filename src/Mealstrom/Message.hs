module Mealstrom.Message (
    Message (..),
    InitRequest (..),
    InitResponse (..),
    getInitId,
    mkInitResponse,
    msgToString,
    MessageId,
    CommonMsg (..),
) where

import Data.Aeson (FromJSON, ToJSON, encode, object, parseJSON, toJSON, withObject, (.:))
import Data.Aeson.Decoding (eitherDecode)
import Data.Aeson.Types ((.=))
import Data.ByteString.Lazy.Char8 qualified as C8
import Data.Text (Text)
import GHC.Natural (Natural)

{-
{src: "c1",
 dest: "n1",
 body: {msg_id: 1,
        type: "init",
        node_id: "n1",
        node_ids: ["n1"]}}

{src: "n1",
 dest: "c1",
 body: {msg_id: 123
        in_reply_to: 1
        type: "init_ok"}}
-}

data Message a = Message -- TODO: split Message to request and response?
    { src :: Text
    , dst :: Text
    , body :: a
    }
    deriving stock (Show)

type MessageId = Integer
data InitRequest
    = InitRequest
        MessageId -- message ID
        Text -- message type
        Text -- node ID
        [Text] -- node IDs
    deriving stock (Show)

data InitResponse
    = InitResponse
        MessageId -- message ID
        MessageId -- in reply to
    deriving stock (Show)

getInitId :: Message InitRequest -> Text
getInitId m =
    let (InitRequest _ _ nodeId _) = body m
     in nodeId

mkInitResponse ::
    Message InitRequest ->
    MessageId ->
    Message InitResponse
mkInitResponse (Message from to (InitRequest initMId _ _ _)) respId =
    Message to from (InitResponse respId initMId)

-- * Serialization
instance ToJSON a => ToJSON (Message a) where
    toJSON (Message _src _dest _body) =
        object
            [ "src" .= _src
            , "dest" .= _dest
            , "body" .= _body
            ]

instance FromJSON a => FromJSON (Message a) where
    parseJSON = withObject "Message" $ \v ->
        Message
            <$> v
            .: "src"
            <*> v
            .: "dest"
            <*> v
            .: "body"

instance ToJSON InitRequest where
    toJSON (InitRequest msg_id msg_type node_id node_ids) =
        object
            [ "msg_id" .= msg_id
            , "type" .= msg_type
            , "node_id" .= node_id
            , "node_ids" .= node_ids
            ]

instance FromJSON InitRequest where
    parseJSON = withObject "InitRequest" $ \v ->
        InitRequest
            <$> v
            .: "msg_id"
            <*> v
            .: "type"
            <*> v
            .: "node_id"
            <*> v
            .: "node_ids"

instance ToJSON InitResponse where
    toJSON (InitResponse msg_id in_reply_to) =
        object
            [ "msg_id" .= msg_id
            , "in_reply_to" .= in_reply_to
            , "type" .= ("init_ok" :: Text)
            ]

msgToString :: ToJSON a => Message a -> String
msgToString = C8.unpack . encode

newtype CommonMsg = CommonMsg {cmMsgId :: Integer}
    deriving stock (Show)

instance FromJSON CommonMsg where
    parseJSON = withObject "CommonMsg" $ \v ->
        CommonMsg <$> v .: "msg_id"
