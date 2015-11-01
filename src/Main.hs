import Data.Tree
import IndentParser
import Parser
import Parsing
import qualified Lexer
import qualified Tokens

-- Convert the AST into a rosetree, for printing.
tree :: AST -> Tree String
tree (Add a b) = Node "'+'" [tree a, tree b]
tree (Argument var_type name) = Node (show var_type ++ " " ++ name) []
tree (Assignment a b) = Node "':='" [tree a, tree b]
tree (Char c) = Node (show c) []
tree (Div a b) = Node "'/'" [tree a, tree b]
tree (Input a b) = Node "'?'" [tree a, tree b]
tree (Integer i) = Node (show i) []
tree (Mul a b) = Node "'*'" [tree a, tree b]
tree (Output a b) = Node "'!'" [tree a, tree b]
tree (Parallel as) = Node "PAR" (map tree as)
tree (Procedure name args body) =
  Node ("PROC " ++ name) [Node "args" (map tree args), tree body]
tree (Program ps) = Node "Program" (map tree ps)
tree (Sequence as) = Node "SEQ" (map tree as)
tree Skip = Node "SKIP" []
tree Stop = Node "STOP" []
tree (String s) = Node (show s) []
tree (Sub a b) = Node "'-'" [tree a, tree b]
tree (Variable x) = Node ("VAR " ++ x) []

print_ast :: AST -> IO ()
print_ast = putStrLn . drawTree . tree

-- Run the lexer!
main = do
  chars <- getContents
  let raw_tokens = Tokens.tokens Lexer.read_token chars
  let tokens = parse_indent raw_tokens
  let x = full_parse program tokens
  putStrLn . (++"\n") . show $ x
  print_ast x
