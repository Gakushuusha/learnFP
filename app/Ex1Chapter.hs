module Ex1Chapter where

factorial num
  | num == 0 = 1
  | num > 0 = num * factorial (num - 1)

fibonacci num
  | num == 0 = 0
  | num == 1 = 1
  | num > 1 = fibonacci(num - 1) + fibonacci(num - 2)

--curry' and uncurry' -- manual currying and uncurrying

-- author's implementation
uncurriedAddition nums =
  let  
    a = fst nums
    b = snd nums
  in a + b

--currying and uncurrying for 2 arguments
curry' f a b = f (a, b)
uncurry' f (a, b) = f a b

uncurriedAddition' = uncurry' (+)
addition' = curry' uncurriedAddition'
addOne = addition' 1
addTwo = addition' 2


