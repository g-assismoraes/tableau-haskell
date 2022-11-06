module Main where

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' _ [] 
    = ([], [])
splitAt' n (x : xs)
    = if n == 1
      then ([], x : xs)
      else (x : xs', xs'')
  where
    (xs', xs'') = splitAt' (n - 1) xs

removeInconveniences :: String -> String
removeInconveniences " " = ""
removeInconveniences a = [e | e<-a, e /= ' ', e /= '-']

splitAux:: Int -> Int -> Int -> [Char] -> Int
splitAux _ _ _  [] = -1
splitAux a b i (x:xs) 
    | x == '&' && a == (b+1) = i
    | x == '|' && a == (b+1) = i
    | x == '>' && a == (b+1) = i
    | x == '(' = splitAux (a+1) b (i+1) xs
    | x == ')' = splitAux a (b+1) (i+1) xs
    | otherwise = splitAux a b (i+1) xs

data Node = 
    Empty 
    | Node {formula :: [String]
           ,esq :: Node
           ,dir :: Node
           ,isLeaf:: Bool
}deriving(Show)


makeTree:: Node -> Node
makeTree Empty = Empty
makeTree a = 
    if (head(head (formula a))) == '~' then Node ((formula a) ++
                                                    [(fst (splitAt (splitAux 0 0 0 (tail  (head(formula a)))) (tail (head(formula a)))))] ++ 
                                                    [(tail (snd(splitAt (splitAux 0 0 0 (tail  (head(formula a)))) (tail (head (formula a))))))])
                                                 Empty
                                                 Empty
                                                 False 
    else 
        Node [" "] Empty Empty True

 --splitAt (splitAux 0 0 0 (tail  (formula a))) (tail (formula a))
        

main :: IO()
main = do
    putStrLn "Insira uma formula logica"
    
    entrada <- getLine

    let a = removeInconveniences ("~(" ++ entrada ++ ")")

    let node = Node [a] Empty Empty True
    print (head(head (formula node)))
    let tree = makeTree node 
    print a
    print tree