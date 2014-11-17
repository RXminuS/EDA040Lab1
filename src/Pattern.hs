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
-- both lists are empty
match _ [] [] = Just []
-- pattern is empty, the other list is not
match _ [] _ = Nothing
-- the  pattern list is not empty, the other list is
match _ _ [] = Nothing
-- two non empty lists
match wc (p:ps) (x:xs)
    -- | first element matches wildcard
    | wc /= p = (if p /= x then Nothing else match wc ps xs)
    -- | first element does not match wildcard
    | otherwise = let swm = singleWildcardMatch (p:ps) (x:xs)
                      lwm = longerWildcardMatch (p:ps) (x:xs)
                  in orElse swm lwm

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = Nothing
{- TO BE WRITTEN -}
longerWildcardMatch (wc:ps) (x:xs) = Nothing
{- TO BE WRITTEN -}

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
transformationApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}

