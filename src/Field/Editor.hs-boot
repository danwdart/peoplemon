module Field.Editor where

import FRP.Yampa
import           Data.Point2
import           Data.Vector2

import Controls
import Field.Terrain
import OfflineData

fieldEditor :: SF (Controls, Vector2 Double, Terrain) (OfflineIO, Terrain)
