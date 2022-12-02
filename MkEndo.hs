import Data.Monoid

mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo collection = Endo $ \val -> foldr ($) val collection
