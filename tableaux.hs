{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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

checaEnt:: Int -> Int -> String -> Int
checaEnt _ _  [] = 0
checaEnt a b (x:xs)
    | x == '&' && a == b = 1
    | x == '|' && a == b = 1
    | x == '>' && a == b = 1
    | x == '(' = checaEnt (a+1) b xs
    | x == ')' = checaEnt a (b+1) xs
    | otherwise = checaEnt a b xs

splitAux:: Int -> Int -> Int -> String -> Int
splitAux _ _ _  [] = -1
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

graftTree:: Node -> Node -> Node
graftTree Empty b = b 
graftTree a Empty = a
graftTree a b 
    | isEmpty (esq a) = Node (formula a) (esq b) (dir b)
    | otherwise = Node (formula a) (graftTree (esq a) b) (graftTree (dir a) b)

data Node =
    Empty
    | Node {formula :: [String]
           ,esq :: Node
           ,dir :: Node
}deriving(Show)


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

 --splitAt (splitAux 0 0 0 (tail  (formula a))) (tail (formula a))


main :: IO()
main = do
    putStrLn "Insira uma formula logica"

    entrada <- getLine

    let a = removeInconveniences (if checaEnt 0 0 entrada == 0 then
                                     "~" ++ entrada 
                                 else 
                                    "~(" ++ entrada ++ ")")


    let node = Node [a] Empty Empty
    let tree = makeTree node
    print a
    print tree