module App.FixedMatrix72 where

import Data.Vec
import Data.Array (mapWithIndex, zip)
import Data.Maybe (fromJust)
import Data.Typelevel.Num (class Lt, class Nat, D1, D2, D6, D7, d0, d1, d2, d3, d4, d5, d6, d7, reifyInt, (==))
import Data.Typelevel.Num.Sets (toInt)
import Partial.Unsafe (unsafeCrashWith)
import Prelude (($), (<>), (<<<), show, class Eq, class Show, (&&))

-- | Matrix of fixed size 7 columns x 2 rows
-- | Flexibility in size might be in order when other Mancala boards will be added.
newtype FixedMatrix72 a =
  FixedMatrix72 (Vec D2 (Vec D7 a))

type Row = forall n. (Nat n, Lt n D2) => n
type Col = forall n. (Nat n, Lt n D7) => n

a :: Row
a = d0
b :: Row
b = d1

newtype Ref = Ref { row :: Row, col :: Col }

instance eqRef :: Eq Ref where
  eq (Ref r1) (Ref r2) = 
    r1.row == r2.row && r1.col == r2.col 

-- derive instance showRef :: (Show Row, Show Col) => Show Ref 

init :: forall a. a -> FixedMatrix72 a
init e =
  FixedMatrix72 $ replicate d2 row 
    where row = replicate d7 e

makeRef :: Row -> Col -> Ref
makeRef row col = Ref { row, col }

getRow :: forall a. Row -> FixedMatrix72 a -> Vec D7 a
getRow row (FixedMatrix72 m) = m !! row

lookup :: forall a. Ref -> FixedMatrix72 a -> a
lookup (Ref ref) (FixedMatrix72 m) = m !! ref.row !! ref.col

modify :: forall a. Ref -> (a -> a) -> FixedMatrix72 a -> FixedMatrix72 a
modify (Ref ref) f (FixedMatrix72 m) =
  FixedMatrix72 $ g' m
    where 
      g' :: Vec D2 (Vec D7 a) -> Vec D2 (Vec D7 a)
      g' = modifyAt ref.row f'
      f' :: Vec D7 a -> Vec D7 a
      f' = modifyAt ref.col f

mapRowWithIndex :: forall a b. Row -> (Ref -> a -> b) -> FixedMatrix72 a -> Array b
mapRowWithIndex row f m = mapWithIndex g arr 
  where arr = toArray (getRow row m)
        g idx e = f (makeRef row col) e
            where col = intToCol idx

intToCol :: forall n. (Nat n, Lt n D7) => Int -> n
intToCol 0 = d0
intToCol 1 = d1
intToCol 2 = d2
intToCol 3 = d3
intToCol 4 = d4
intToCol 5 = d5
intToCol 6 = d6
intToCol 7 = d7
