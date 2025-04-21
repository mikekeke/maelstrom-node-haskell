module Mealstrom (spawnNode) where

import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Mealstrom.Node (Node, ReqHandler)
import Mealstrom.Node qualified as Node

spawnNode ::
  ( FromJSON req
  , ToJSON resp
  ) =>
  ReqHandler req resp ->
  IO Node
spawnNode =
  Node.spawnNode
