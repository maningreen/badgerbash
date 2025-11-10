module Types.WidgetID where

data WidgetID = WidgetID () | Special Int | Button ButtonType
  deriving (Eq, Ord)

data ButtonType = SetTime Int | SetWords Int | Exit | Restart
  deriving (Eq, Ord)
