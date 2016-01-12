import Data.Tree
import IndentParser
import Parser
import Parsing
import Semantics
import qualified Lexer
import qualified Tokens

-- Run the lexer!
main = do
  chars <- getContents
  let raw_tokens = Tokens.tokens Lexer.read_token chars
  let tokens = parse_indent raw_tokens
  let x = full_parse process tokens
  (res, state) <- run_analyser (check_process x)
  putStrLn . ("\nFinal Tree:\n\n" ++) . show $ res
  putStrLn . ("\n\nFinal State:\n\n" ++) . show $ state
