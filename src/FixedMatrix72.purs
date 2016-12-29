module App.FixedMatrix72 where

import Matrix as Matrix
import Data.Array (mapWithIndex)
import Data.Maybe (fromJust)
import Matrix (Matrix)
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, class Show, show, ($), (&&), (<$>), (<<<), (<>), (==))

-- | Matrix of fixed size 7 columns x 2 rows
-- | Flexibility in size might be in order when other Mancala boards will be added.
newtype FixedMatrix72 a =
  FixedMatrix72 (Matrix a)

data Row = A | B

instance eqRow :: Eq Row where
  eq A A = true
  eq B B = true
  eq _ _ = false

instance showRow :: Show Row where
  show A = "A"
  show B = "B"

newtype Ref = Ref { row :: Row, idx :: Int }

instance eqRef :: Eq Ref where
  eq (Ref r1) (Ref r2) =
    r1.row == r2.row && r1.idx == r2.idx

instance showRef :: Show Ref where
  show (Ref { row, idx }) =
    "Ref row=" <> show row <> " idx=" <> show idx

init :: forall a. a -> FixedMatrix72 a
init e = FixedMatrix72 $ Matrix.repeat 7 2 e

makeRef :: Row -> Int -> Ref
makeRef row idx = Ref { row, idx }

getRow :: forall a. Row -> FixedMatrix72 a -> Array a
getRow row (FixedMatrix72 m) = unsafePartial fromJust v
  where v = Matrix.getRow (rowToInt row) m

lookup :: forall a. Ref -> FixedMatrix72 a -> a
lookup (Ref ref) (FixedMatrix72 m) = unsafePartial fromJust v
  where v = Matrix.get col row m
        row = rowToInt ref.row
        col = ref.idx

modify :: forall a. Ref -> (a -> a) -> FixedMatrix72 a -> FixedMatrix72 a
modify (Ref ref) f (FixedMatrix72 m) = FixedMatrix72 $ unsafePartial fromJust v
  where v = Matrix.modify ref.idx (rowToInt ref.row) f m

mapRowWithIndex :: forall a b. Row -> (Ref -> a -> b) -> FixedMatrix72 a -> Array b
mapRowWithIndex row f = mapWithIndex f' <<< getRow row
  where f' = f <<< makeRef row

getValues :: forall a. FixedMatrix72 a -> Array a
getValues (FixedMatrix72 m) = _.value <$> Matrix.toIndexedArray m
        
rowToInt :: Row -> Int
rowToInt A = 0
rowToInt B = 1
