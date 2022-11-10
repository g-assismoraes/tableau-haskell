{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

removeInconveniences :: String -> String
removeInconveniences " " = ""
removeInconveniences a = [e | e<-a, e /= ' ', e /= '-']

checkEnt:: Int -> Int -> String -> Int
checkEnt _ _  [] = 0
checkEnt a b (x:xs)
    | x == '&' && a == b = 1
    | x == '|' && a == b = 1
    | x == '>' && a == b = 1
    | x == '(' = checkEnt (a+1) b xs
    | x == ')' = checkEnt a (b+1) xs
    | otherwise = checkEnt a b xs

splitAux:: Int -> Int -> Int -> String -> Int
splitAux _ _ _  [] = error "A entrada inserida apresenta alguma inconsistência!"
splitAux a b i (x:xs)
    | x == '&' && a == b+1 = i
    | x == '|' && a == b+1 = i
    | x == '>' && a == b+1 = i
    | x == '(' = splitAux (a+1) b (i+1) xs
    | x == ')' = splitAux a (b+1) (i+1) xs
    | otherwise = splitAux a b (i+1) xs

isEmpty:: Node -> Bool
isEmpty Empty = True
isEmpty _ = False

data Node =
    Empty
    | Node {formula :: [String]
           ,esq :: Node
           ,dir :: Node
}deriving(Show)

graftTree:: Node -> Node -> Node
graftTree Empty b = b 
graftTree a Empty = a
graftTree a b 
    | isEmpty (esq a) = Node (formula a) (esq b) (dir b)
    | otherwise = Node (formula a) (graftTree (esq a) b) (graftTree (dir a) b)


makeTree:: Node -> Node
makeTree Empty = Empty
makeTree a
  | length (head (formula a)) <= 2 = a
  | head(head (formula a)) == '~' &&
    head(tail(head (formula a))) == '(' =
         case tail (head (formula a)) !! splitAux 0 0 0 (tail  (head(formula a))) of
            '|' -> Node (formula a ++
                            formula(makeTree(
                                Node ["~" ++ tail(take (splitAux 0 0 0 (tail  (head(formula a)))) (tail (head(formula a))))]
                                (esq a)
                                (dir a))) ++
                            formula(makeTree(
                                Node ["~" ++ init(tail (drop (splitAux 0 0 0 (tail  (head(formula a)))) (tail (head (formula a)))))]
                                (esq a)
                                (dir a))))
                        (graftTree (esq a) (esq (graftTree (makeTree( Node ["~" ++ tail(take (splitAux 0 0 0 (tail  (head(formula a)))) (tail (head(formula a))))]
                                                                            Empty
                                                                            Empty))
                                (makeTree(Node ["~" ++ init(tail (drop (splitAux 0 0 0 (tail  (head(formula a)))) (tail (head (formula a)))))]
                                         Empty
                                         Empty)))))
                        (graftTree (dir a) (dir (graftTree(makeTree( Node ["~" ++ tail(take (splitAux 0 0 0 (tail  (head(formula a)))) (tail (head(formula a))))]
                                                                    Empty
                                                                    Empty))
                                (makeTree(Node ["~" ++ init(tail (drop (splitAux 0 0 0 (tail  (head(formula a)))) (tail (head (formula a)))))]
                                         Empty
                                         Empty)))))                       
            '>' -> Node (formula a ++
                            formula(makeTree(
                                Node [tail(take (splitAux 0 0 0 (tail  (head(formula a)))) (tail (head(formula a))))]
                                (esq a)
                                (dir a))) ++
                            formula(makeTree(
                                Node ["~" ++ init(tail (drop (splitAux 0 0 0 (tail  (head(formula a)))) (tail (head (formula a)))))]
                                (esq a)
                                (dir a))))
                        (graftTree (esq a) (esq (graftTree (makeTree( Node [tail(take (splitAux 0 0 0 (tail  (head(formula a)))) (tail (head(formula a))))]
                                                                     Empty
                                                                     Empty))
                                (makeTree(Node ["~" ++ init(tail (drop (splitAux 0 0 0 (tail  (head(formula a)))) (tail (head (formula a)))))]
                                         Empty
                                         Empty)))))
                        (graftTree (dir a) (dir (graftTree(makeTree( Node [tail(take (splitAux 0 0 0 (tail  (head(formula a)))) (tail (head(formula a))))]
                                                                     Empty
                                                                     Empty))
                                (makeTree(Node ["~" ++ init(tail (drop (splitAux 0 0 0 (tail  (head(formula a)))) (tail (head (formula a)))))]
                                         Empty
                                         Empty))))) 
            '&' -> Node (formula a)
                        (graftTree (esq a)
                                   (makeTree(Node ["~" ++ tail(take (splitAux 0 0 0 (tail  (head(formula a)))) (tail (head(formula a))))]
                                            Empty
                                            Empty)))
                        (graftTree (dir a)
                                   (makeTree(Node ["~" ++ init(tail (drop (splitAux 0 0 0 (tail  (head(formula a)))) (tail (head (formula a)))))]
                                            Empty
                                            Empty)))
  | head(head (formula a)) == '~' && head(tail(head (formula a))) == '~' =
     Node (formula a ++ formula (makeTree (Node [drop 2 (head (formula a))] (esq a) (dir a))))
          (esq (makeTree (Node [drop 2 (head (formula a))] (esq a) (dir a)))) 
          (dir (makeTree (Node [drop 2 (head (formula a))] (esq a) (dir a))))
  | otherwise =
        case head (formula a) !! splitAux 0 0 0 (head(formula a)) of
            '&' -> Node (formula a ++
                            formula(makeTree(
                                Node [tail(take (splitAux 0 0 0 ((head(formula a)))) ((head(formula a))))]
                                (esq a)
                                (dir a))) ++
                            formula(makeTree(
                                Node [init(tail (drop (splitAux 0 0 0 ((head(formula a)))) ((head (formula a)))))]
                                (esq a)
                                (dir a))))
                        (graftTree (esq a) (esq (graftTree (makeTree( Node [tail(take (splitAux 0 0 0 ((head(formula a)))) ((head(formula a))))]
                                                                      Empty
                                                                      Empty))
                                (makeTree(Node [init(tail (drop (splitAux 0 0 0 ((head(formula a)))) ((head (formula a)))))]
                                         Empty
                                         Empty)))))
                        (graftTree (dir a) (dir (graftTree(makeTree( Node [tail(take (splitAux 0 0 0 ((head(formula a)))) ((head(formula a))))]
                                                                     Empty
                                                                     Empty))
                                (makeTree(Node [init(tail (drop (splitAux 0 0 0 ((head(formula a)))) ((head (formula a)))))]
                                         Empty
                                         Empty)))))    
            '>' -> Node (formula a)
                        (graftTree (esq a)
                                   (makeTree(Node ["~" ++ tail(take (splitAux 0 0 0 ((head(formula a)))) ((head(formula a))))]
                                            Empty
                                            Empty)))
                        (graftTree (dir a)
                                   (makeTree(Node [init(tail (drop (splitAux 0 0 0 ((head(formula a)))) ((head (formula a)))))]
                                            Empty
                                            Empty)))
            '|' -> Node (formula a)
                        (graftTree (esq a)
                                   (makeTree(Node [tail(take (splitAux 0 0 0 ((head(formula a)))) ((head(formula a))))]
                                            Empty
                                            Empty)))
                        (graftTree (dir a)
                                   (makeTree(Node [init(tail (drop (splitAux 0 0 0 ((head(formula a)))) ((head (formula a)))))]
                                            Empty
                                            Empty)))

printBlockAux :: Int -> String -> String
printBlockAux 0 _ = ""
printBlockAux 1 b = b ++ "        "
printBlockAux a b = printBlockAux (a-1) (b ++ "          ")

printBlock:: Int -> [String] -> String -> String -> IO()
printBlock _ [] _ _ = putStr ""
printBlock i f a v = 
         if i == length f then do
            putStr a
            putStrLn (head f)
            printBlock i (tail f) a v
        else do
            putStr v
            putStrLn (head f)
            printBlock i (tail f) a v


drawTreeAux :: Int -> String -> String
drawTreeAux 0 _ = ""
drawTreeAux 1 b = b ++ "[-----  "
drawTreeAux a b = drawTreeAux (a-1) (b ++ "          ")


drawTree:: Node -> Int -> IO()
drawTree Empty andar = do
    putStr (drawTreeAux andar "          ")
    print Empty
drawTree a andar = do
    drawTree (esq a) (andar + 1)
    printBlock ((div (length(formula a)) 2) + 1)  (formula a) (drawTreeAux andar "          ") (printBlockAux andar "          ")
    drawTree (dir a) (andar + 1)


findAtomsAux:: [String] -> [[String]] -> [[String]]
findAtomsAux [] v = v
findAtomsAux f v = 
    if length (head f) <= 2 then
         findAtomsAux (tail f) [(head v) ++ [head f]] 
    else findAtomsAux (tail f) v


findAtoms:: Node -> [[String]] -> [[String]] 
findAtoms a v 
    | not (isEmpty (esq a)) = findAtoms (esq a) (findAtomsAux (formula a) v) ++ findAtoms (dir a) (findAtomsAux (formula a) v)
    | otherwise = findAtomsAux (formula a) v


itHasContradition:: [String] -> Bool
itHasContradition a
    | length([e | e <-a, length e == 1, elem ("~" ++ e) a]) > 0 = True
    | otherwise = False 
    

evaluateTree:: [[String]] -> [Bool]
evaluateTree [] = [False]
evaluateTree a = [itHasContradition e |e<-a]


fetchInterpretation:: [String] -> [String]
fetchInterpretation [] = []
fetchInterpretation a = [e | e <- a, length e == 1, not(elem ("~" ++ e) a)] ++ [e | e <- a, length e == 2, not(elem (tail e) a)]


fetchInterpretations:: [[String]] -> [[String]]
fetchInterpretations [] = []
fetchInterpretations a = [if (fetchInterpretation e) == [] then ["Ramo fechado!"] else e | e<-a]


leavesWithNoContradition:: [Bool] -> [Int] -> Int -> [Int]
leavesWithNoContradition [] _ _ = []
leavesWithNoContradition (x:xs) r c 
    |not x = r ++ [c] ++ leavesWithNoContradition xs r (c+1)
    |x = r ++ leavesWithNoContradition xs r (c+1)
    |otherwise = leavesWithNoContradition xs r (c+1)


isValid:: Node -> IO()
isValid tree = do
    let idxs = (leavesWithNoContradition (evaluateTree (findAtoms tree [[]])) [] 1)
    if length idxs == 0 then do putStrLn "A fórmula é valida!"
    else do 
        putStr "A fórmula não é válida. Confira as interpretações da análise dos ramos:  " 
        print (fetchInterpretations (findAtoms tree [[]]))

tableau:: IO()
tableau = do
    -- (p|(q&r))>((p|q)&(p|r)) 
    -- a -> (a -> (b -> a))
    -- b -> (a & (b & a))
    -- (x&~x)|(y&~y)

    putStrLn "Insira uma fórmula lógica: "

    entrada <- getLine

    let a = removeInconveniences (if checkEnt 0 0 entrada == 0 then
                                     "~" ++ entrada 
                                 else 
                                    "~(" ++ entrada ++ ")")


    let node = Node [a] Empty Empty
    let tree = makeTree node

    print (leavesWithNoContradition (evaluateTree (findAtoms tree [[]])) [] 1)

    putStrLn "---------------------------------------------------------------------------------------------------------------------------------"
    drawTree tree 0
    putStrLn "---------------------------------------------------------------------------------------------------------------------------------"
    isValid tree


main :: IO()
main = do
    tableau
    