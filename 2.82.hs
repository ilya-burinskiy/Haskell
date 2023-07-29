data Type = Scheme Integer | Rational Integer Integer | Complex Integer Integer

typeCasts = [Just (\ (Scheme x) -> Scheme x), Nothing, Nothing]

result = [Scheme 1, Rational 1 2, Complex 3 4]

