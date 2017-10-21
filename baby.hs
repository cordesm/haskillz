doubleMe x = x + x

doubleUs x y = x*2 + y*2


doubleSmallNumber x = if x > 100
  then x
  else doubleMe x

myLast:: [a]->a
myLast [] = error "no end for empty lists"
myLast [x] = x
myLast (_:xs) = myLast xs

myPenultimate :: [a]->a
myPenultimate [] = error "no penulitmate for empty"
myPenultimate [x] = error "no penulitmate for one"
myPenultimate [x, y] = x
myPenultimate (_:xs) = myPenultimate (xs)

myNth :: [a]->Int->a
myNth (x:_) 1 = x
myNth [] _ = error "bad stuff"
myNth (_:xs) n = myNth xs (n-1)
-- don't know how to deal with when n<1

length' :: [a]->Int
length' [] = 0
length' (x:xs) = 1 + length'(xs)

myInit :: [a] -> [a]
myInit [x] = []
myInit (x:xs) = x:myInit(xs)

myReverse :: [a]->[a]
myReverse [] = []
myReverse xs = myLast(xs):myReverse (myInit xs)
