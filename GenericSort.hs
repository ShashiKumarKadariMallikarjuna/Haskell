-- Shashi Kumar Kadari Mallikarjuna
-- CECS 424 Assignment 3
-- Generic sort in Haskell
 

import Data.List

--main function
main = do
  --assiging the list of numbers to the numbers variable
  let numbers = [645.32,37.40,76.30,5.40,-34.23,1.11,-34.94,23.37,635.46,-876.22,467.73,62.26]
  --assigning the list of people to the people variable
  let people = [("Hal", 20), ("Susann", 31), ("Dwight", 19), ("Kassandra", 21), ("Lawrence", 25), ("Cindy", 22), ("Cory", 27), ("Mac", 19), ("Romana", 27), ("Doretha", 32), ("Danna", 20), ("Zara", 23), ("Rosalyn", 26), ("Risa", 24), ("Benny", 28), ("Juan", 33), ("Natalie", 25)]
  
  putStrLn " "       --next line
  print ("Sorting numbers ascending by numerical value: ")
  print (sortBy compare numbers )  --prints the sorted list of numbers in ascending order
  
  putStrLn " "           --next line
  print("Sorting people alphabetically(lexicographically) by name: ")
  print (sortBy compareAlpha people)    --prints the sorted list of people in ascending order based on names
  
  putStrLn " "            --next line
  print("Sorting people decending by age, where people of the same age are sorted alphabetically: ")
  print (sortBy compareAge people)       --prints the sorted list of people in decending order based on age and then with names if the age is the same
  
--function that compares the names of people
compareAlpha (personName1,_) (personName2,_) = compare personName1 personName2

--function that compares the age of the people and then names if the age of two people is the same
compareAge (personName1,age1) (personName2,age2) = 
  if(age1 == age2)
   then compare personName1 personName2
   else (flip compare) age1 age2

