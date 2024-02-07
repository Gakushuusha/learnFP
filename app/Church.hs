module Church where

-- Church numerals after implementing id, const and (.) and after eta-reduction

type Church a = (a -> a) -> a -> a
zero :: Church a
zero  = const id

one :: Church a
one = id

two :: Church a
two f = (f . f)

three :: Church a
three f = (f . two f)

--convertation

toChurch :: Int -> Church a
toChurch 0 = \f -> id
toChurch i = \f x -> f (toChurch (i - 1) f x)


fromChurch :: Church Int -> Int
fromChurch a = a (+ 1) 0

-- Various signatures for incrementing by 1
inc x = x + 1
inc2 x = (+) x 1
inc3 = (+ 1)

-- increments a Church-encoded number by one
plusOne :: Church a -> Church a
plusOne b f x = (f . b f) x

add :: Church a -> Church a -> Church a
add a b f x = (a f . b f) x

mul :: Church a -> Church a -> Church a
mul a b f  = a (b f)


true a b = const a b
false a b = b

churchAnd a b = a (b false)

churchOr a b = a true b

churchNot a = a false true

churchIf l a b = l a b

-- Church encoding of numerals before implementing id, const and (.)
-- and before eta-reduction

-- type Church a = (a -> a) -> a -> a
-- zero :: Church a
-- zero f x = x
-- one :: Church a
-- one f x = f x
-- two :: Church a
-- two f x = f (f x)
-- three :: Church a
-- three f x = f (two f x)


-- toChurch :: Int -> Church a
-- toChurch 0 = \f x -> x
-- toChurch i = \f x -> f (toChurch (i - 1) f x)

--toChurch :: Int -> Church a
--toChurch 0 = zero
--toChurch i = plusOne (toChurch (i - 1))

-- two inc3 0

-- fromChurch :: Church Int -> Int
-- fromChurch a = a (+ 1) 0

-- inc x = x + 1
-- inc2 x = (+) x 1
-- inc3 = (+ 1)

-- plusOne :: Church a -> Church a
-- plusOne b f x = f (b f x)

-- add :: Church a -> Church a -> Church a
-- add a b f x = a f (b f x)

-- mul :: Church a -> Church a -> Church a
-- mul a b f x = a (b f) x
