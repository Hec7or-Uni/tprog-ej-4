----- Data Birth ---------------------
data Birth = BBY Int | ABY Int 
    deriving Eq

instance Show Birth where
    show (BBY a) = show a ++ " " ++ "BBY"
    show (ABY a) = show a ++ " " ++ "ABY"

instance Ord Birth where
    (<) (ABY a) (ABY b) = a > b 
    (<) (BBY a) (BBY b) = a < b 
    (<) (BBY a) (ABY b) = False
    (<) (ABY a) (BBY b) = True

    (<=) (ABY a) (ABY b) = a >= b 
    (<=) (BBY a) (BBY b) = a <= b 
    (<=) (BBY a) (ABY b) = False
    (<=) (ABY a) (BBY b) = True


----- Data People --------------------
data Person = Person String Birth 
    deriving Eq

instance Show Person where
    show (Person name date) = name ++ " - " ++ show date

instance Ord Person where
    (<)  (Person na da) (Person nb db) = da <  db
    (<=) (Person na da) (Person nb db) = da <= db


----- Data N-Tree --------------------
data FamilyTree = Empty | Leaf Person | Node Person [FamilyTree]
    deriving Show

----- Youngest -----------------------
youngest :: FamilyTree -> String
youngest t = n
    where
        (Person n _) = youngest' t

youngest' :: FamilyTree -> Person
youngest' (Empty)  = undefined
youngest' (Leaf p) = p
youngest' (Node p [])  = p
youngest' (Node p bsq) = min p (youngest'' bsq)

youngest'' :: [FamilyTree] -> Person
youngest'' (t:[]) = youngest' t
youngest'' (t:ts) = min (youngest' t) (youngest'' ts)


----- Main ---------------------------
main = do
    let starWars =  Node (Person "shmi" (BBY 72)) [Node (Person "Anakin" (BBY 42)) [Node (Person "Luke" (BBY 19)) [Leaf (Person "Ben" (ABY 26))], Node (Person "Leia" (BBY 19)) [Leaf (Person "Jaina" (ABY 9)), Leaf (Person "Jacen" (ABY 9)), Leaf (Person "Ben" (ABY 1)), Leaf (Person "Anakin" (ABY 10))]]]
    print $ youngest starWars