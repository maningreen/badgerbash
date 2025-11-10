module Button where

import Types.WidgetID
import Brick (Widget, str, clickable)

data Button = Button {
    _label :: String
  , _id :: WidgetID
  }

compileButtons :: [Button] -> [Widget WidgetID]
compileButtons xs = map f xs
  where
    f :: Button -> Widget WidgetID
    f button = clickable (_id button) . str $ _label button
