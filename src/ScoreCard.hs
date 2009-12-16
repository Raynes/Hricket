module ScoreCard where

-- TODO rewrite this code so that Card is actually a Map and an Int (for score.

data Mark = None | One | Two | Closed | Open
            deriving (Show)

data Card = Card {fift  :: Mark
                 ,sixt  :: Mark
                 ,sevt  :: Mark
                 ,eigt  :: Mark
                 ,nint  :: Mark
                 ,twet  :: Mark
                 ,bull  :: Mark
                 ,score :: Int}
            deriving (Show)

setMark :: [Char] -> Card -> Mark
setMark n = case n of
              "15"   -> fift
              "16"   -> sixt
              "17"   -> sevt
              "18"   -> eigt
              "19"   -> nint
              "20"   -> twet
              "bull" -> bull

changeMark :: Card -> (String, Mark) -> Card
changeMark c (s,m) = c {}


--mark :: Card -> [(String, Mark)] -> Card
--mark c (s,m) = c {sfunc = 