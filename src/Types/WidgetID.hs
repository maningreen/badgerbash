module Types.WidgetID where

import Util (Time)

data WidgetID = WidgetID () | Special Int | Button ButtonType
  deriving (Eq, Ord)

data ButtonType = SetTime Time | SetWords Int | Exit | Restart
  deriving (Eq, Ord)
