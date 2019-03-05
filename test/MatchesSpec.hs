module MatchesSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Patterns
import qualified Data.Text as T

spec :: Spec

spec = do
    describe "Patterns.charMatches (Matcher Any)" $ do
        it "Matches with lower case letter" $ do
            charMatches 'b' Any
        it "Matches with upper case letter" $ do
            charMatches 'V' Any
        it "Matches with brackets" $ do
            charMatches '{' Any
        it "Matches with whitespace" $ do
            charMatches '\r' Any

    describe "Patterns.charMatches (Matcher Char c)" $ do
        it "Matches with the given character" $ do
            charMatches 'x' (Char 'x')
        it "Does not match the wrong character" $ do
            charMatches '5' (Char '!') == False
        it "Is still correct over whitespace" $ do
            charMatches '.' (Char '\n') == False

    describe "Patterns.charMatches (Matcher OneOf text)" $ do
        it "Matches when the character is at the end" $ do
            charMatches 'h' (OneOf $ T.pack "abcdefgh")
        it "Matches when the character is at the start" $ do
            charMatches '1' (OneOf $ T.pack "12345")
        it "Matches when the character is in the middle" $ do
            charMatches '^' (OneOf $ T.pack "@$^*(!x")
        it "Matches with different classes of character in the text" $ do
            charMatches ' ' (OneOf $ T.pack "r5%4<n\rx \nE1~")
        it "Does not match when not in text" $ do
            charMatches 'B' (OneOf $ T.pack "yuiopl") == False