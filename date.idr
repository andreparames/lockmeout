module Date

import Data.Vect

data LeapYear = Yes | No

numDays : LeapYear -> Int -> Maybe Int
numDays _    1 = Just 31
numDays Yes  2 = Just 29
numDays No   2 = Just 28
numDays _    3 = Just 31
numDays _    4 = Just 30
numDays _    5 = Just 31
numDays _    6 = Just 30
numDays _    7 = Just 31
numDays _    8 = Just 31
numDays _    9 = Just 30
numDays _   10 = Just 31
numDays _   11 = Just 30
numDays _   12 = Just 31
numDays _    _ = Nothing

isLeapYear : (year : Int) -> LeapYear
isLeapYear year = case (((mod year 4) == 0) &&
  not ((mod year 100) == 0)) of
  True => Yes
  False => case ((mod year 400) == 0) of
             True => Yes
             False => No

record Date where
  constructor MkDate
  year, month, day : Int

public export
rjust : (s : Vect ns Char) -> (n : Nat) -> (fillchar : Char) -> {auto prf : LTE ns n} -> Vect n Char
rjust {ns} s n fillchar = Data.Vect.drop ns (rewrite plusCommutative ns n in (Data.Vect.replicate n fillchar) ++ s)

-- rjust : (s : String) -> (n : Nat) -> (fillchar : Char) -> String
-- rjust s n fillchar = pack $ toList $ rjust' (fromList (unpack s)) n fillchar

vecToString : Vect n Char -> String
vecToString v = pack (toList v)

formatDay'' : (l : Vect n Char) -> (m : Nat) -> Maybe (Vect m Char)
formatDay'' l m {n} with (isLTE n m)
  | Yes prf = Just $ rjust l m '0' {prf}
  | No _    = Nothing

formatDay' : String -> (m : Nat) -> Maybe (String)
formatDay' s m = do
  v <- formatDay'' (fromList (unpack s)) m
  pure $ pack $ toList v

formatDay : (day : Nat) -> String
formatDay day = case (formatDay' (show day) 2) of
                     Just fmtDay => fmtDay
                     Nothing     => "XX"

implementation Show Date where
  show date = (show $ year date) ++ "-" ++ (formatDay (month date)) ++ "-"

validDate : (year : Int) -> (month : Int) -> (day : Int) -> Maybe Date
validDate year month day = do
    max <- numDays (isLeapYear year) month
    case ((day > 0) && (day <= max)) of
      True => Just $ MkDate year month day
      False => Nothing

parseDateStr : String -> Maybe Date
parseDateStr str = do
  let parts = split (== '-') str
  year  <- index' 0 parts
  month <- index' 1 parts
  day   <- index' 2 parts
  validDate 
    (cast {from=String} {to=Int} year)
    (cast {from=String} {to=Int} month)
    (cast {from=String} {to=Int} day)
