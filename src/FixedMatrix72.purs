module App.FixedMatrix72 where

import Matrix as Matrix
import Matrix (Matrix)
import Prelude (($))
import Partial.Unsafe (unsafePartial)
import Data.Maybe (fromJust)

-- | Matrix of fixed size 7 columns x 2 rows
newtype FixedMatrix72 a = 
  FixedMatrix72 (Matrix a)

data Row = A | B

init :: forall a. a -> FixedMatrix72 a
init e =
  FixedMatrix72 $ Matrix.repeat 7 2 e

getRow :: forall a. Row -> FixedMatrix72 a -> Array a
getRow row (FixedMatrix72 m) = 
  unsafePartial fromJust $ Matrix.getRow (rowToInt row) m

rowToInt :: Row -> Int 
rowToInt A = 0
rowToInt B = 1