import Text.XHtml (hidden)
import GHC.IO.Handle (hSetEcho)
import System.IO ( hSetEcho, stdin )
-- Hangman in haskell

outputStringLn :: String -> IO ()
outputStringLn [] = putChar '\n'
outputStringLn (x:xs) = do
                            putChar x
                            outputStringLn xs

inputString :: IO String
inputString = do
                x <- getChar
                if x == '\n' then 
                    return []
                else
                    do
                        xs <- inputString
                        return (x:xs)

hiddenInputChar :: IO Char
hiddenInputChar = do 
                    hSetEcho stdin False -- Disables std output
                    x <- getChar
                    hSetEcho stdin True
                    return x 

hiddenInputString :: IO String 
hiddenInputString = do
                        x <- hiddenInputChar
                        if x == '\n' then 
                            return []
                        else
                            do
                                xs <- hiddenInputString
                                return (x:xs)

hangman :: IO ()
hangman = do
            outputStringLn "Think of a word: "
            word <- hiddenInputString
            outputStringLn "Try to guess it: "
            play word (initalGuess (length word))

initalGuess :: Int -> String
initalGuess 0 = []
initalGuess count = '-' : initalGuess (count-1)

play :: String -> String -> IO ()
play word currentGuess =
    do
        guess <- inputString

        if length guess == length word then
            if guess == word then 
                outputStringLn "You got the word!"
            else
                do
                    outputStringLn (compareStrings word guess currentGuess)
                    play word (compareStrings word guess currentGuess)
        else
            do
                outputStringLn "Not correct number of characters try again"
                play word currentGuess

compareStrings :: String -> String -> String -> String
compareStrings xs ys zs = retStr 
                        where 
                            cmpStr = [if x `elem` ys then x else '-' | x <- xs]
                            retStr = compareStringGuess cmpStr zs

compareStringGuess :: String -> String -> String
compareStringGuess [] [] = []
compareStringGuess (x:xs) (y:ys)
                | x == y = x : compareStringGuess xs ys
                | x /= '-' = x : compareStringGuess xs ys
                | y /= '-' = y : compareStringGuess xs ys
