module ModuleXOR where

xor :: Bool -> Bool -> Bool
xor x y | x == True && y == False = True
        | x == False && y == True = True
        | otherwise = False

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False

xor'' :: Bool -> Bool -> Bool
xor'' True a = not a
xor'' False a = a
