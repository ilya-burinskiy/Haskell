class Printable a where
    toString :: a -> String

instance Printable Bool where
    toString False = "false"
    toString True = "true"

instance Printable () where
    toString () = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
    toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"
