-- decode (one | two | three) (hundred | thousand)  
decode = \c -> c 0
as x = \c -> c x
a x = \c -> c x
number = id

one x = \c -> c (x + 1)
two x = \c -> c (x + 2)
three x = \c -> c (x + 3)
seventeen x = \c -> c (x + 17)
twenty x = \c -> c (x + 20)
hundred m = \c -> c (m * 100)
thousand m = \c -> c (m * 1000)
