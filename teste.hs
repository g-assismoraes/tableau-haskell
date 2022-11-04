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


-- stringCount str ch = [ y | (x, y) <- zip str [0..], x == ch ]


-- -- ~([[a,&,b],|,[c,&,b]],>,[[a,&,b],>, b])
-- --     (a&b)>(a | b)


-- esq:: Int -> Int
-- esq ind = ind*2 + 1

-- dir:: Int -> Int
-- dir ind = ind*2 + 2

-- pai:: Int -> Int
-- pai ind = div (ind - 1) 2

-- findFormula:: [] -> []
-- findFormula

-- -- [[["~a>b"], ["~a"], ["~b"], ["~g"], ["~d"], ["d"], ["g"], ["~p"], ["~e"]], ["p"], ["r"], ["p"], ["q"], ["p"], ["q"], ["~q"], ["~r"], ]

-- ((a&(a>b))|(c|d))>e


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

    let a = "~(" ++ entrada ++ ")"

    let node = Node [a] Empty Empty True
    print (head(head (formula node)))
    let tree = makeTree node 
    print a
    print tree


    -- let c = splitAux 0 0 0 entrada

    -- let r = splitAt c entrada

    -- print r

    -- --let a = "~(" ++ entrada ++ ")" 

    -- --makeTree(a)

    -- let node = Node ["a->b", "a&p"] (Node ["a"] Empty Empty True) Empty False
    -- let e = esq node
    -- print node
    -- print e
    

    -- let a = entrada !! 0
    -- let f = tail (snd r)
    -- print f
    -- print a


