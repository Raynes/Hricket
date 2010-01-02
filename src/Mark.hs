module Mark where

data Mark = None | One | Two | Three
            deriving (Show,Eq,Ord)

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

instance Read Mark where
    readsPrec _ value = 
        tryParse [("0", None),("1", One), ("2", Two), ("3", Three)]
        where tryParse [] = []
              tryParse ((attempt, result):xs) =
                      if take (length attempt) value == attempt
                         then [(result, drop (length attempt) value)]
                         else tryParse xs