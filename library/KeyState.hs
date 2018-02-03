module KeyState where

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