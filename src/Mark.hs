module Mark where

data Mark = None | One | Two | Three
            deriving (Show,Eq,Ord,Read)

instance Num Mark where
  (+) None x   = x
  (+) x None   = x
  (+) One One  = Two
  (+) Three _ = Three
  (+) _ Three = Three
  (+) One Two  = Three
  (+) Two One  = Three
  (+) Two Two  = Three
  
  fromInteger 0 = None
  fromInteger 1 = One
  fromInteger 2 = Two
  fromInteger 3 = Three