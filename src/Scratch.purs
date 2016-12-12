module App.Scratch where 

-- We are using https://pursuit.purescript.org/packages/purescript-sized-vectors/1.0.0
import Data.Typelevel.Num (class Lt, class Nat, toInt, D2, D7)
import Data.Vec (Vec, modifyAt)
import Prelude (($))

newtype FixedMatrix72 a = FixedMatrix72 (Vec D2 (Vec D7 a))

newtype Row = Row forall n. (Nat n, Lt n D2) => n
newtype Col = Col forall n. (Nat n, Lt n D7) => n
newtype Ref = Ref { row :: Row, col :: Col }

instance natRow :: Nat Row where 
  toInt (Row r) = toInt r

modify :: forall a. Ref -> (a -> a) -> FixedMatrix72 a -> FixedMatrix72 a
modify (Ref ref) f (FixedMatrix72 m) =
  FixedMatrix72 $ modifyAt ref.row (modifyAt ref.col f) m
