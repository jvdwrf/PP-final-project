module Main where

import Compiler (compile)
import qualified Control.Exception as Exc
import Sprockell.Simulation (run)
import System.Environment.Blank (getArgs)

---- Compiles a number into a spril program producing all fibonacci numbers below the number
---- Compilation might fail
--compile :: String -> Either String [Instruction]
--compile txt = do
--    ast <- parseMyLang txt
--    pure $ codeGen ast

-- Gets a number and runs the resulting spril program of compilation succeeds
main :: IO ()
main =
  Exc.catch
    ( do
        args <- getArgs
        let fileName = head args
        program <- readFile fileName
        let compiled = compile program
        putStr
          (
              "\n-----------------------------------------------------------------------------\n"
              ++ program
              ++ "\n-----------------------------------------------------------------------------\n"
          )
        run compiled
        putStr "-----------------------------------------------------------------------------\n"
    )
    handler
  where
    handler :: Exc.ErrorCall -> IO ()
    handler exc = putStrLn ("\nError: \n" ++ show exc)
