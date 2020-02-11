--main function  
main = do
  let list = [4,65,2,-31,0,99,2,83,782,1]     --initializing the list of numbers which have to be sorted
  let result = msort list                     --calling the msort sort fuction on the list and assigning the sorted list to result
  print "Sorted list using merge sort:"       --print the string  
  print result                                --print the sorted array  
  let list2 = [4,65,2,-31,0,99,2,83,782,1]    --initializing the list of numbers which have to be sorted
  let result2 = qsort list2                    --calling the qsort sort fuction on the list and assigning the sorted list to result2
  print "Sorted list using quick sort:"       --print the string 
  print result2                               --print the sorted array

--merge sort a list 
msort :: Ord a => [a] -> [a]
msort [] = []                            --if msort is called on empty list, it returns emptylist
msort [x] = [x]                          --if there is a list, it returns the list as it is
msort xs = merge (msort ys) (msort zs)   -- merge is called on the two sub-lists ys and zs
  where                                  --"where"
    (ys,zs) = split xs                   --ys and zs sublists are returned by the function split applied on xs 

-- Split a list into two halves
split :: (Ord a) => [a] -> ([a],[a])
--splits the list into two halves ->first half is taken by using 'take' which takes only first xs elements 
--and the second half is taken by using 'drop' which drops the first xs elements and takes the remaining elements from the original list
split xs = (take len xs, drop len xs)     
  where                                  --"where"   
    len = length xs `div` 2              --mentions that the length of the sub-lists
  
-- Expects xs and ys to already be sorted
merge :: (Ord a) => [a] -> [a] -> [a]  -- merge two sorted lists to a sorted list
merge xs [] = xs                       -- if one
merge [] ys = ys                       -- or the other list is empty merging is trivial
merge (x:xs) (y:ys)                    -- merge two non-empty lists
  | x <= y     = x:(merge xs (y:ys))   -- if the left element is smaller then output it first
  | otherwise  = y:(merge (x:xs) ys)   --otherwise output the right element first


--quick sort a list
qsort :: (Ord a) => [a] -> [a]
qsort [] = []                               --if msort is called on empty list, it returns emptylist
qsort [x] = [x]                             --if there is a list, it returns the list as it is
--qsort is called on the left and right sublist recursively and it is concatenated with the pivot in the middle
--(the list left of pivot(x) would have elements less than the pivot and the list right of the pivot would have elements greater than the pivot)
qsort (x:xs) = qsort [y | y <- xs , y<=x] ++[x] ++ qsort [y | y <- xs ,y>x]





