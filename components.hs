{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
import Control.Lens.TH
import Data.These
import Control.Lens
import Data.Bifunctor
import Data.Profunctor

data Component state down up where
  Component
    :: Tree state down up
    -> Component state down up

data Tree state down up where
  Slot
    :: Traversal' parentDown childDown
    -> Review parentUp childUp
    -> Traversal' parentState childState
    -> Component childState childDown childUp
    -> Tree parentState parentDown parentUp
  Mappend
    :: Tree state down up
    -> Tree state down up
    -> Tree state down up
  Ceiling
    :: (up -> Filter (Behavior down up))
    -> Tree state down up
    -> Tree state down up
  Floor
    :: (down -> Filter (Behavior down up))
    -> Tree state down up
    -> Tree state down up

data Filter a = Passthrough a | Keep a

data Behavior down up
  = SendUp [up]
  | SendDown [down]
  | SendUpAndDown [up] [down]
  | SendNothing

data ButtonUp
  = Click
  | DblClick

data ButtonDown
  = SetTitle String
  | SetDisabled Bool

data Button =
  Button
    { buttonTitle :: String
    }

button :: String -> Component Button ButtonDown ButtonUp
button = undefined

data TextDown =
  SetText String

data Text =
  Text
    { textText :: String
    }

text :: Component Text TextDown ()
text = undefined

data PageDown
  = ButtonDown ButtonDown
  | TextDown TextDown
  | Reset

$(makePrisms ''PageDown)

data PageUp
  = ButtonUp ButtonUp
  | TextUp ()

$(makePrisms ''PageUp)

data Page =
  Page
    { _pageButton :: Button
    , _pageText :: Text
    }

$(makeLenses ''Page)

page :: Tree Page PageDown PageUp
page =
  Ceiling
    (\case
       ButtonUp Click ->
         SendDown
           [ ButtonDown (SetTitle "Click me again!")
           , TextDown (SetText "Clicked!")
           ]
       _ -> SendNothing)
    (Mappend
       (Slot _ButtonDown _ButtonUp pageButton (button "Click me!"))
       (Floor
          (\case
             Reset -> SendDown [TextDown (SetText "Resetted.")])
          (Slot _TextDown _TextUp pageText text)))
