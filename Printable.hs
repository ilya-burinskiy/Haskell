class Printable a where
    toString :: a -> String

instance Printable Bool where
    toString False = "false"
    toString True = "true"

instance Printable () where
    toString () = "unit type"
