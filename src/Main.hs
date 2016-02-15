import CodeGen
import Data.Tree
import IndentParser
import Parser
import Parsing
import SemanticAnalyser
import Semantics
import qualified Lexer
import qualified Tokens

-- Run the lexer!
main = do
  chars <- getContents
  let raw_tokens = Tokens.tokens Lexer.read_token chars
  let tokens = parse_indent raw_tokens

  let x = full_parse process tokens
  (proc, state) <- run_analyser (check_process x)

  let space_allocated = -head (static_chain state)
  putStrLn ("  # Local space allocated: " ++ show space_allocated)
  assemble (space_allocated + 1) proc
  -- let x = full_parse process tokens
  -- (res, state) <- run_analyser (check_process x)
  -- putStrLn . ("\nFinal Tree:\n\n" ++) . show $ res
  -- putStrLn . ("\nFinal State:\n\n" ++) . show $ state
