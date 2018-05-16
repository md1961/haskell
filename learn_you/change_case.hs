import Data.Char (isLower, isUpper, toLower, toUpper)
import Data.Traversable (sequenceA)
import Data.List (intercalate)

isLegalChar :: Char -> Bool
isLegalChar c = isLower c || isUpper c || c `elem` "_-"

hasIllegalChar :: String -> Bool
hasIllegalChar = not . all isLegalChar

isCamel :: String -> Bool
isCamel = any isUpper

isSnake :: String -> Bool
isSnake = ('_' `elem`)

isKebab :: String -> Bool
isKebab = ('-' `elem`)

isMixed :: String -> Bool
isMixed s = (length $ filter id $ sequenceA [isCamel, isSnake, isKebab] s) >= 2

wordsDecamelize :: String -> [String]
wordsDecamelize "" = []
wordsDecamelize s = [word] ++ wordsDecamelize rest
    where strDecap = toLower (head s) : tail s
          word = takeWhile isLower strDecap
          rest = dropWhile isLower strDecap

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn delim s = [word] ++ splitOn delim rest
    where word = takeWhile (/= delim) s
          rest = dropWhile (== delim) $ dropWhile (/= delim) s

toWords :: String -> Maybe [String]
toWords s
    | hasIllegalChar s || isMixed s = Nothing
    | isCamel s = Just $ wordsDecamelize s
    | isSnake s = Just $ splitOn '_' s
    | isKebab s = Just $ splitOn '-' s
    | otherwise = Just [s]

joinCamelizing :: [String] -> Maybe String
joinCamelizing xs = Just $ decapitalize $ concat $ map capitalize xs
    where capitalize   s = toUpper (head s) : (tail s)
          decapitalize s = toLower (head s) : (tail s)

changeCase :: String -> String -> Maybe String
changeCase "" _      = Just ""
changeCase s "camel" = toWords s >>= joinCamelizing
changeCase s "snake" = toWords s >>= (Just . intercalate "_")
changeCase s "kebab" = toWords s >>= (Just . intercalate "-")
changeCase _ _       = Nothing

main = do
    print $ changeCase "snakeCase" "snake"
    print $ changeCase "some-lisp-name" "camel"
    print $ changeCase "map_to_all" "kebab"
    print $ changeCase "doHTMLRequest" "kebab"
    print $ changeCase "invalid-inPut_bad" "kebab"
    print $ changeCase "valid-input" "huh???"
    print $ changeCase "" "camel"
