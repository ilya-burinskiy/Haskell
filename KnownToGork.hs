class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab ork
        | doesEnrageMork ork && doesEnrageGork ork = stomp (stab ork)
        | doesEnrageMork ork = stomp ork
        | doesEnrageGork ork = stab ork
        | otherwise = ork
