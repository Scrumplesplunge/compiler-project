import CodeGen
import qualified Data.ByteString.Lazy as BL
import Data.Tree
import IndentParser
import Parser
import Parsing
import SemanticAnalyser hiding (putStdErr)
import Semantics
import Static
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
  assembler_file :: String,
  data_file :: String,
  memory_start :: Integer
}

defaults = Options {
  input_file = Nothing,
  assembler_file = "code.s",
  data_file = "data.bin",
  memory_start = 0x80000070
}

is_valid :: Options -> Bool
is_valid options = (input_file options /= Nothing)

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
            ("--assembler_file" : v : args') -> do
              -- Set the output assembler file.
              read_args' args' (options { assembler_file = v })
            ("--data_file" : v : args') -> do
              -- Set the output data file.
              read_args' args' (options { data_file = v })
            ("--memory_start" : v : args') -> do
              -- Set the address where available memory starts.
              read_args' args' (options { memory_start = read v })
            ("--source_file" : input : args') -> do
              check_unset "source file" (input_file options) input
              read_args' args' (options { input_file = Just input })
            (x : xs) -> error $ "Error with command-line option: " ++ show x

open :: String -> IOMode -> Handle -> IO Handle
open "-" m h = return h
open f m h = openFile f m

main = do
  options <- read_args

  let input_filename =
        case input_file options of
          Nothing -> error "No input file specified."
          Just x -> x

  input_handle <- open input_filename ReadMode stdin

  chars <- hGetContents input_handle
  let raw_tokens = Tokens.tokens Lexer.read_token chars
  let tokens = parse_indent raw_tokens

  let x = full_parse process tokens
  let state = empty_state { next_static_address = memory_start options }
  (proc, state') <- run_analyser (check_process x) state

  -- let x = full_parse process tokens
  -- (res, state) <- run_analyser (check_process x)
  -- putStrLn . ("\nFinal Tree:\n\n" ++) . show $ res
  -- putStrLn . ("\nFinal State:\n\n" ++) . show $ state

  if num_errors state' > 0 then
    hPutStrLn stderr ("Compilation failed with " ++ show (num_errors state') ++
                      " error(s) and " ++ show (num_warnings state') ++
                      " warning(s).")
  else do
    -- Write assembler file.
    assembler_handle <- open (assembler_file options) WriteMode stdout
    let static_size = next_static_address state' - memory_start options
    assemble (context { stack_depth = static_size }) proc assembler_handle
    hClose assembler_handle

    -- Write data file.
    data_handle <- open (data_file options) WriteMode stdout
    let blob = make_blob (static state')
    BL.hPut data_handle blob
    hClose data_handle
