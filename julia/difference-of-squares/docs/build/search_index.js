var documenterSearchIndex = {"docs":
[{"location":"#Difference-of-Squares","page":"Difference of Squares","title":"Difference of Squares","text":"","category":"section"},{"location":"","page":"Difference of Squares","title":"Difference of Squares","text":"DocTestSetup = :(using DifferenceOfSquares)","category":"page"},{"location":"","page":"Difference of Squares","title":"Difference of Squares","text":"square_of_sum","category":"page"},{"location":"#DifferenceOfSquares.square_of_sum","page":"Difference of Squares","title":"DifferenceOfSquares.square_of_sum","text":"square_of_sum(n::Integer)::Integer\n\nSquare the sum of the first n positive integers.\n\nbeginalign*\n  left(sum_x=1^n xright)^2\n  = left(fracn(n + 1)2right)^2 \n  = fracn^2 + 2n^3 + n^44\nendalign*\n\n\n\n\n\n","category":"function"},{"location":"","page":"Difference of Squares","title":"Difference of Squares","text":"sum_of_squares","category":"page"},{"location":"#DifferenceOfSquares.sum_of_squares","page":"Difference of Squares","title":"DifferenceOfSquares.sum_of_squares","text":"sum_of_squares(n::Integer)::Integer\n\nSum the squares of the first n positive integers.\n\nbeginalign*\n  sum_x=1^n x^2\n  = fracn(n + 1)(2n + 1)6 \n  = fracn + 3n^2 + 2n^36\nendalign*\n\n\n\n\n\n","category":"function"},{"location":"","page":"Difference of Squares","title":"Difference of Squares","text":"difference","category":"page"},{"location":"#DifferenceOfSquares.difference","page":"Difference of Squares","title":"DifferenceOfSquares.difference","text":"difference(n::Integer)::Integer\n\nSubtract the sum of squares from square of the sum of the first n positive integers.\n\nbeginalign*\n  left(sum_x=1^n xright)^2 - sum_x=1^n x^2\n  = left(fracn(n + 1)2right)^2 - fracn(n + 1)(2n + 1)6 \n  = fracn^2 + 2n^3 + n^44 - fracn + 3n^2 + 2n^36 \n  = frac-2n - 3n^2 + 2n^3 + 3n^412\nendalign*\n\n\n\n\n\n","category":"function"}]
}