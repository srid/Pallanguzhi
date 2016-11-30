module App.FixedMatrix72 where

import Matrix as Matrix
import Data.Maybe (fromJust)
import Matrix (Matrix)
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
  where v = Matrix.get col row m
        row = rowToInt ref.row
        col = ref.idx
  
modify :: forall a. Ref -> (a -> a) -> FixedMatrix72 a -> FixedMatrix72 a
modify (Ref ref) f (FixedMatrix72 m) =
  FixedMatrix72 $ unsafePartial fromJust v
  where v = Matrix.modify (rowToInt ref.row) ref.idx f m

rowToInt :: Row -> Int 
rowToInt A = 0
rowToInt B = 1

