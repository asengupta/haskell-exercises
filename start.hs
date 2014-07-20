triple x = x*3
add x y = x+y
addsTo x y total = if (x+y == total) then True else False
evenOrOdd x = (if x`mod`2==1 then "odd" else "even") ++ " yes, kid"

recur cnt lastNumber = if cnt > lastNumber then [] else cnt:(recur (cnt+1) lastNumber)
upto lastNumber = recur 1 lastNumber
weird n = [ if x > 10 then "BANG!" else "BOOM!" | x <- [1..n], x `mod` 2 == 1 ]

triangle :: Int -> [(Int, Int, Int)]
triangle maxSide = [(a,b,c) | a <- [1..maxSide], b <- [1..maxSide], c <- [1..maxSide], a*a + b*b == c*c]

factorial n = if n == 1 then 1 else n*(factorial (n-1))
factorial' :: (Integral a) => a -> a
factorial' 1 = 1
factorial' n = n * (factorial' (n-1))

prophet :: (Integral a) => a -> String
prophet 7 = "That's good, that's good"
prophet x = "Not correct"

addVector::(Num a)=>(a,a) -> (a,a) -> (a,a)
addVector x y = ((fst x) + (fst y), (snd x) + (snd y))

addVector'::(Num a)=>(a,a) -> (a,a) -> (a,a)
addVector' (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

xCoord::(Num a) => (a,a) -> a
xCoord (x,_) = x

yCoord::(Num a) => (a,a) -> a
yCoord (_,y) = y

head'::[a]->a
head' (x:xs) = x
head' [] = error "Loool"

firstTwo::[a]->[a]
firstTwo [] = error "Empty list"
firstTwo [x] = error "Only one element"
firstTwo (x:y:xs) = [x,y]

count::[a] -> Int
count [] = 0
count (_:xs) = 1 + count xs

sum'::(Num a)=>[a]->a
sum' [] = 0
sum' (x:xs) = x + (sum xs)

upto'::(Integral a)=>a->a->[a]
upto' x upper 
	| x > upper = []
	| otherwise = x:(upto' (x+1) upper)


age::(Integral a)=>a->String
age a
	| calculatedAge == 0 = "Unborn"
	| calculatedAge <= 400 = "Young"
	| calculatedAge <= 800 = "MIddle Age"
	| otherwise = "Old"
	where calculatedAge = a*20

zip'::[a]->[a]->[(a,a)]
zip' (x1:x1s) (x2:x2s) = (x1, x2):(zip' x1s x2s)
zip' [] _ = []
zip' _ [] = []

last'::[a]->a
last' [] = error "Empty list"
last' [x] = x
last' (x:xs) = last' xs

penultimate::[a]->a
penultimate []=error "Empty list"
penultimate [x]=error "Only one element"
penultimate (x:_:[])=x
penultimate (_:xs)=penultimate xs

at::[a]->(Int->a)
at (x:_) 1=x
at [] _=error "Array index out oof bounds"
at (x:xs) index=at xs (index-1)

rev::[a]->[a]
rev []=[]
rev (x:xs)=(rev xs)++[x]

rev''::[a]->[a]
rev'' list=internalRev list [] where
	internalRev [] result=result
	internalRev (x:xs) result=internalRev xs (x:result)

isPalindrome::(Eq a)=>[a]->Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome list
	| (f /= lastElement) = False
	| otherwise = isPalindrome (shortened list)
	where (f:_) = list;
		lastElement = last list;
		shortened l=init (tail l);


zipWith'::(a->b->c)->[a]->[b]->[c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith' f xs ys)
