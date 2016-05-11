import AnnotatedAST
import CodeGen
import qualified Data.ByteString.Lazy as BL
import Data.Int
import IndentParser
import qualified MetaData
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
  assembly_file :: String,
  metadata_file :: String,
  memory_start :: Int32
}

defaults = Options {
  input_file = Nothing,
  assembly_file = "code.s",
  metadata_file = "metadata.json",
  memory_start = two_pow_31
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
            ("--assembly_file" : v : args') -> do
              -- Set the output assembler file.
              read_args' args' (options { assembly_file = v })
            ("--metadata_file" : v : args') -> do
              -- Set the output metadata file.
              read_args' args' (options { metadata_file = v })
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

  if num_errors state' > 0 then
    hPutStrLn stderr ("Compilation failed with " ++ show (num_errors state') ++
                      " error(s) and " ++ show (num_warnings state') ++
                      " warning(s).")
  else do
    -- Write assembler file.
    let static_end = next_static_address state'
    let static_size = static_end - memory_start options

    assembler_handle <- open (assembly_file options) WriteMode stdout
    max_depth <- assemble context proc assembler_handle
    hClose assembler_handle

    -- Space for stack and process descheduling. Static data is not included.
    let space_needed = 4 * (6 + max_depth)

    -- Write metadata file.
    let (static_data_start, static_data) = make_blob (static state')
    if static_data_start /= memory_start options then
      error "Compiler error: static data does not start at memory start."
    else do
      -- Workspace descends. Initially place the workspace pointer at the top of
      -- allocated memory.
      let metadata = MetaData.MetaData {
            MetaData.static_data = static_data,
            MetaData.root_process_size = space_needed,
            MetaData.assembly_file = assembly_file options}
      metadata_handle <- open (metadata_file options) WriteMode stdout
      hPutStr metadata_handle (MetaData.encode metadata)
      hClose metadata_handle
