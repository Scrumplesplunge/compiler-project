import CodeGen
import Data.Tree
import IndentParser
import Parser
import Parsing
import SemanticAnalyser hiding (putStdErr)
import Semantics
import System.Environment
import System.IO
import qualified Lexer
import qualified Tokens

-- Helper for stderr.
putStdErr :: String -> IO ()
putStdErr = hPutStrLn stderr

-- Compilation options.
data Options = Options {
  input_file :: Maybe String,
  output_file :: Maybe String
}

defaults = Options {
  input_file = Nothing,
  output_file = Nothing
}

is_valid :: Options -> Bool
is_valid options =
  input_file options /= Nothing && output_file options /= Nothing

check_unset :: Show a => String -> Maybe a -> a -> IO ()
check_unset name option val = do
  case option of
    Nothing ->
      return ()
    Just x ->
      putStdErr ("Warning: Overriding " ++ name ++ " (" ++ show x ++ " -> " ++
                 show val ++ ").")

read_args :: IO Options
read_args = do
  args <- getArgs
  read_args' args defaults
  where read_args' args options =
          case args of
            [] -> return options
            ("--output" : v : args') -> do
              -- Set the output file.
              check_unset "output file" (output_file options) v
              read_args' args' (options { output_file = Just v })
            (input : args') -> do
              check_unset "input file" (input_file options) input
              read_args' args' (options { input_file = Just input })

open :: String -> IOMode -> Handle -> IO Handle
open "-" m h = return h
open f m h = openFile f m

main = do
  options <- read_args

  let input_filename =
        case input_file options of
          Nothing -> error "No input file specified."
          Just x -> x
  let output_filename =
        case output_file options of
          Nothing -> error "No output file specified."
          Just x -> x

  input_handle <- open input_filename ReadMode stdin

  chars <- hGetContents input_handle
  let raw_tokens = Tokens.tokens Lexer.read_token chars
  let tokens = parse_indent raw_tokens

  let x = full_parse process tokens
  (proc, state) <- run_analyser (check_process x)

  -- let x = full_parse process tokens
  -- (res, state) <- run_analyser (check_process x)
  -- putStrLn . ("\nFinal Tree:\n\n" ++) . show $ res
  -- putStrLn . ("\nFinal State:\n\n" ++) . show $ state

  if num_errors state > 0 then
    hPutStrLn stderr ("Compilation failed with " ++ show (num_errors state) ++
                      " error(s) and " ++ show (num_warnings state) ++
                      " warning(s).")
  else do
    output_handle <- open output_filename WriteMode stdout
    assemble context proc output_handle
    hClose output_handle
