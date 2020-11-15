module Value where

data Value
  = Brainfuck String
  | Number Int
  | Function ([Value] -> Value)

instance Show Value where
  show (Brainfuck bf) = show bf
  show (Number n) = show n
  show (Function _) = "<function>"

instance Eq Value where
  Brainfuck bf == Brainfuck bf' = bf == bf'
  Number n == Number n' = n == n'
  Function _ == Function _ = False  -- hmm
  _ == _ = False

asBrainfuck :: Value -> Maybe String
asBrainfuck (Brainfuck bf) = Just bf
asBrainfuck _ = Nothing

asFunction :: Value -> Maybe ([Value] -> Value)
asFunction (Function f) = Just f
asFunction _ = Nothing
