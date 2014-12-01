module Pattern where
import Utilities


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute wc (x:xs) s
    | wc == x = s ++ (substitute wc xs s)
    | otherwise = x:(substitute wc xs s)

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match wc pattern@(p:ps) string@(x:xs)
    -- | first element matches wildcard
    -- | wc /= p = (if p /= x then Nothing else match wc ps xs)
    -- | first element does not match wildcard
    -- | otherwise = orElse swm lwm
    -- where swm = singleWildcardMatch pattern string
    --      lwm = longerWildcardMatch pattern string
    --
     | wc == p = orElse swm lwm
     | p == x = match wc ps xs
     | otherwise = Nothing
     where swm = singleWildcardMatch pattern string
           lwm = longerWildcardMatch pattern string

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = mmap (const [x]) $ match wc ps xs
--singleWildcardMatch (wc:ps) (x:xs) = let mtc = match wc ps xs
--                                     in (if mtc /= Nothing then (Just [x]) else Nothing)

longerWildcardMatch pattern@(wc:_) (x:xs) = mmap (x:) $ match wc pattern xs

-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions

-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
--transformationApply wc f xs (ps1,ps2) = let mtc = match wc ps1 xs
--                                            mtc2 = mmap f mtc
--                                            g = substitute wc ps2
--                                       in mmap g mtc2
transformationApply wc f xs (ps1,ps2) = mmap ((substitute wc ps2).f) (match wc ps1 xs)


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
--transformationsApply _ _ [] _ = Nothing
--transformationsApply wc f (p:ps) xs = orElse tf tfs
--    where tf = transformationApply wc f xs p
--          tfs = transformationsApply wc f ps xs
transformationsApply wc f ps xs = foldr1 (\x acc -> orElse x acc) (map (transformationApply wc f xs) ps) 
