module App.FixedMatrix72 where

import Matrix as Matrix
import Data.Maybe (fromJust)
import Matrix (Matrix, set)
import Partial.Unsafe (unsafePartial)
import Prelude (($))

-- | Matrix of fixed size 7 columns x 2 rows
-- | Flexibility in size might be in order when other Mancala boards will be added.
newtype FixedMatrix72 a = 
  FixedMatrix72 (Matrix a)

data Row = A | B

newtype Ref = Ref { row :: Row, idx :: Int }

init :: forall a. a -> FixedMatrix72 a
init e =
  FixedMatrix72 $ Matrix.repeat 7 2 e

makeRef :: Row -> Int -> Ref
makeRef row idx = Ref { row, idx }

getRow :: forall a. Row -> FixedMatrix72 a -> Array a
getRow row (FixedMatrix72 m) = 
  unsafePartial fromJust v
  where v = Matrix.getRow (rowToInt row) m

lookup :: forall a. Ref -> FixedMatrix72 a -> a
lookup (Ref ref) (FixedMatrix72 m) =
  unsafePartial fromJust v 
  where v = Matrix.get (rowToInt ref.row) ref.idx m
  
set :: forall a. Ref -> a -> FixedMatrix72 a -> FixedMatrix72 a
set (Ref ref) value (FixedMatrix72 m) =
  FixedMatrix72 $ unsafePartial fromJust v
  where v = Matrix.set (rowToInt ref.row) ref.idx value m

rowToInt :: Row -> Int 
rowToInt A = 0
rowToInt B = 1

