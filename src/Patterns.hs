module Patterns (Pattern(..), Matcher(..), TokenResult(..), getNextToken, parseText, charMatches, matchPattern) where
    
import qualified Data.Text as T
import qualified Data.Char as C

data Matcher = Any | Char C.Char | OneOf T.Text 
    deriving (Eq, Show)

data Pattern = Match Matcher | Repeat Pattern Int | RepeatMost Pattern Int |
    Sequence Pattern Pattern | Fork Pattern Pattern | Until Matcher | While Matcher |
    Bottom
    deriving (Eq, Show)

data TokenResult = Result T.Text T.Text | None
    deriving (Eq, Show)

parseText :: T.Text -> [Pattern] -> [T.Text]
parseText text patterns 
    | T.length text <= 0 = []
    | otherwise = case getNextToken text patterns of 
                        None -> []
                        Result token rest -> token : (parseText rest patterns)
                                                    

getNextToken :: T.Text -> [Pattern] -> TokenResult
getNextToken _ [] = None
getNextToken text (p:ps) =
    let result = matchPattern text [] p
    in case result >>= (\x -> Just $ T.pack (reverse x)) of 
        Nothing -> getNextToken text ps
        Just match -> resOrNothing match text
    where resOrNothing m t = case T.stripPrefix m t of Nothing -> None
                                                       Just strip -> Result m strip


charMatches :: C.Char -> Matcher -> Bool

charMatches _ Any = True
charMatches c (Char other) = c == other
charMatches c (OneOf string)  
    | T.length string <= 0 = False
    | T.head string == c = True
    | otherwise = charMatches c (OneOf (T.tail string))


matchPattern :: T.Text -> [Char] -> Pattern -> Maybe [Char] 

matchPattern text build (Match m) 
        | T.length text <= 0 = Nothing
        | charMatches (T.head text) m == True = Just $ (T.head text) : build
        | otherwise = Nothing

matchPattern text build (Repeat p n)
        | n <= 0 = Just build
        | T.length text <= 0 = Nothing
        | otherwise = let first = matchPattern text build p
            in first >>= (\x -> matchPattern (T.tail text) x (Repeat p (n-1)))

matchPattern text build (RepeatMost p n)
        | n <= 0 = Just build
        | T.length text <= 0 = Just build
        | otherwise = case matchPattern text build p of 
            Nothing -> Just build
            Just newBuild -> matchPattern (T.tail text) newBuild (RepeatMost p (n-1))

matchPattern text build (Sequence a b) = let resultA = matchPattern text build a
        in resultA >>= (\x -> matchPattern (T.tail text) x b)

matchPattern text build (Fork a b) = case matchPattern text build a of 
    Nothing -> matchPattern text build b
    Just something -> Just something

matchPattern text build (Until m) 
    | T.length text <= 0 = Nothing    
    | otherwise = case matchPattern text build (Match m) of
        Nothing -> matchPattern (T.tail text) ((T.head text) : build) (Until m) 
        Just _ -> Just build 

matchPattern text build (While m)
    | T.length text <= 0 = Nothing
    | otherwise = case matchPattern text build (Match m) of
        Nothing -> Just build
        Just newBuild -> matchPattern (T.tail text) ((T.head text) : build) (While m)

matchPattern _ build Bottom = Just build