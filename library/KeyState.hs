module KeyState
  ( KeyStatus(..)
  , KeyState(..)
  , initKeyState
  , pressedKeyState
  , releasedKeyState
  , updateKeyState
  , maintainKeyState
  , isHeld
  , isPressed
  , isUntouched
  , isReleased
  , isTouched
  ) where

data KeyStatus
  = KeyStatus'Untouched
  | KeyStatus'Pressed
  | KeyStatus'Held
  | KeyStatus'Released
  deriving (Show, Eq, Ord, Enum)

data KeyState count = KeyState
  { ksStatus :: KeyStatus
  , ksCounter :: Maybe count -- ^ Counter
  } deriving (Show, Eq)

initKeyState :: KeyState count
initKeyState = KeyState KeyStatus'Untouched Nothing

pressedKeyState :: KeyState count
pressedKeyState = KeyState KeyStatus'Pressed Nothing

releasedKeyState :: KeyState count
releasedKeyState = KeyState KeyStatus'Released Nothing

updateKeyState
  :: Num count
  => count -- ^ Counter delta
  -> KeyState count
  -> Bool -- ^ Touched
  -> KeyState count
updateKeyState delta KeyState{ksStatus, ksCounter} True = case ksStatus of
  KeyStatus'Untouched -> KeyState KeyStatus'Pressed Nothing
  KeyStatus'Pressed -> KeyState KeyStatus'Held Nothing
  KeyStatus'Held -> KeyState KeyStatus'Held (Just $ delta + (case ksCounter of Nothing -> 0; Just counter -> counter))
  KeyStatus'Released -> KeyState KeyStatus'Pressed Nothing
updateKeyState delta KeyState{ksStatus, ksCounter} False = case ksStatus of
  KeyStatus'Untouched -> KeyState KeyStatus'Untouched (Just $ delta + (case ksCounter of Nothing -> 0; Just counter -> counter))
  KeyStatus'Pressed -> KeyState KeyStatus'Released Nothing
  KeyStatus'Held -> KeyState KeyStatus'Released Nothing
  KeyStatus'Released -> KeyState KeyStatus'Untouched Nothing

maintainKeyState
  :: Num count
  => count -- ^ Counter delta
  -> KeyState count
  -> KeyState count
maintainKeyState delta KeyState{ksStatus, ksCounter} = case ksStatus of
  KeyStatus'Untouched -> KeyState KeyStatus'Untouched (Just $ delta + (case ksCounter of Nothing -> 0; Just counter -> counter))
  KeyStatus'Pressed -> KeyState KeyStatus'Held Nothing
  KeyStatus'Held -> KeyState KeyStatus'Held (Just $ delta + (case ksCounter of Nothing -> 0; Just counter -> counter))
  KeyStatus'Released -> KeyState KeyStatus'Untouched Nothing

isHeld :: KeyState a -> Bool
isHeld ks = ksStatus ks == KeyStatus'Held

isPressed :: KeyState a -> Bool
isPressed ks = ksStatus ks == KeyStatus'Pressed

isUntouched :: KeyState a -> Bool
isUntouched ks = ksStatus ks == KeyStatus'Untouched

isReleased :: KeyState a -> Bool
isReleased ks = ksStatus ks == KeyStatus'Released

isTouched :: KeyState a -> Bool
isTouched ks = isPressed ks || isHeld ks
