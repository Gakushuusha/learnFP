module Ex2Chapter where
    --page 87-88 effective haskell
    reverse' list =
      let 
        accumulateHead accumulateArray elementFromLeft = elementFromLeft : accumulateArray
      in foldl accumulateHead [] list

    -- (╯°益°)╯彡┻━┻
    reverseRight list = 
      let
        accumulateTail accumulateArray elementFromRight =  elementFromRight ++ [accumulateArray]
      in foldr accumulateTail [] list

    --zipWith implementation without list comprehensions
    zipWith' f list1 list2
      | null list1 || null list2 = [] 
      | otherwise = f (head list1) (head list2) : zipWith' f (tail list1) (tail list2)

    --zipWith implementation with list comprehension 
    
    elementAt list index
      | index < length list = Just (list !! index)
      | otherwise = Nothing

    minLength list1 list2 = min (length list1) (length list2)

    zipWithListComprehension f list1 list2 = [f x y | index <- [0..minLength list1 list2 - 1], 
         Just x <- [elementAt list1 index], 
         Just y <- [elementAt list2 index]]

    --zipWith implementation with foldl
    -- (╮°-°)╮┳━━┳

    zipWithFoldl f list1 list2 = foldl step [] indexes
      where
        step acc index = applyAt index acc
        applyAt index acc = case (elementAt list1 index, elementAt list2 index) of
                              (Just x, Just y) -> acc ++ [f x y]
                              _ -> acc
        indexes = [0..minLength list1 list2 - 1]
    




    