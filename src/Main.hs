module Main where

import LanguageImplementation
import ParserG
import DrawShapes
import System.Environment (getArgs)
import System.Directory

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> doesFileExist x >>= \case
                                  True -> do
                                    content <- readFile x
                                    evalTurtle content
                                  False -> putStrLn ("There is no such file < " ++ x ++ " > .")
    []        -> putStrLn "Before you compile, write the name of the source code file!"
    otherwise -> putStrLn "Invalid number of arguments!"

evalTurtle :: String -> IO ()
evalTurtle = runTurtleWithErrorCheck initTurtleState
           . toComp
           . evalListOfCommands initTurtleState
           . runTurtleParser
