module PatternsSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Patterns
import qualified Data.Text as T

spec :: Spec

spec = do
    describe "Patterns.matchPattern (Match m)" $ do
        it "Finds character in pattern" $ do
            matchPattern (T.pack "abc") [] (Match $ Char 'a') `shouldBe` Just ['a']
        it "Returns Nothing if no match" $ do
            matchPattern (T.pack "!73") [] (Match $ Char 'A') `shouldBe` Nothing
        
    describe "Patterns.matchPattern (Repeat p n)" $ do
        it "Finds the correct number" $ do
            case matchPattern (T.pack "11111111") [] (Repeat (Match Any) 4) of
                Just string -> length string == 4
                Nothing -> False
        it "Does not match on a shorter string" $ do
            matchPattern (T.pack "!!!!") [] (Repeat (Match Any) 18) `shouldBe` Nothing 
        it "Finds the correct character" $ do
            matchPattern (T.pack "aaa") [] (Repeat (Match $ Char 'a') 3) `shouldBe` Just "aaa"
        it "Finds the correct sequence (in reverse)" $ do
            matchPattern (T.pack "123456") [] (Repeat (Match Any) 6) `shouldBe` Just "654321"
    
    describe "Patterns.matchPattern (RepeatMost p n)" $ do
        it "Finds the correct number" $ do
            case matchPattern (T.pack "12345") [] (RepeatMost (Match Any) 5) of
                Just string -> length string == 5
                Nothing -> False
        it "Finds fewer if the input text is shorter" $ do
            case matchPattern (T.pack "1234") [] (RepeatMost (Match Any) 15) of
                Just string -> length string == 4
                Nothing -> False
    
    describe "Patterns.matchPattern (Sequence a b)" $ do
        it "Executes patterns in sequence" $ do
            matchPattern (T.pack "ab") [] (Sequence (Match $ Char 'a') (Match $ Char 'b')) `shouldBe` Just "ba"
        it "Fails if the first fails" $ do
            matchPattern (T.pack "$%") [] (Sequence (Match $ Char 'x') (Match Any)) `shouldBe` Nothing
        it "Fails if the second fails" $ do
            matchPattern (T.pack "xy") [] (Sequence (Match Any) (Match $ Char 'D')) `shouldBe` Nothing
        it "Can be chained" $ do
            matchPattern (T.pack "abc") [] (Sequence (Match $ Char 'a') (Sequence (Match $ Char 'b') (Match $ Char 'c'))) `shouldBe` Just "cba"

    describe "Patterns.matchPattern (Fork a b)" $ do
        it "Matches first if first is ok" $ do
            matchPattern (T.pack "$#") [] (Fork (Match Any) (Match Any)) `shouldBe` Just "$"
        it "Matches second if second ok" $ do
            matchPattern (T.pack "XY") [] (Fork (Match $ Char 'A') (Match $ Char 'X')) `shouldBe` Just "X"
        it "Gets nothing if both fail" $ do
            matchPattern (T.pack "abcde") [] (Fork (Match $ Char 'x') (Match $ Char 'y')) `shouldBe` Nothing

    describe "Patterns.matchPattern (Until a b)" $ do
        it "Matches until given condition" $ do
            matchPattern (T.pack "hello world") [] (Until $ Char ' ') `shouldBe` Just "olleh"
        it "Fails if condition is never met" $ do
            matchPattern (T.pack "aaaaaaaaaa") [] (Until $ Char 'X') `shouldBe` Nothing
        it "Matches if until terminates at end of string" $ do
            matchPattern (T.pack "xyz$") [] (Until $ Char '$') `shouldBe` Just "zyx"
    
    describe "Patterns.matchPattern (Bottom)" $ do
        it "Just returns build" $ do
            matchPattern (T.pack "foo bar") "test" Bottom `shouldBe` Just "test"
