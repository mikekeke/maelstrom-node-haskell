module Maelstrom (
    Node.spawnNode,
    Node.waitNode,
    Node.simpleHandler,
) where

import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Maelstrom.Node (Node, ReqHandler)
import Maelstrom.Node qualified as Node

-- spawnNode ::
--   ( FromJSON req
--   , ToJSON resp
--   ) =>
--   ReqHandler req resp ->
--   IO Node
-- spawnNode =
--   Node.spawnNode
