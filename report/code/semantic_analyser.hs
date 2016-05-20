-- Information associated with a defined name.
type NameInfo = (Type, Location)

-- All names defined thus far.
type Environment = [(AST.Name, NameInfo)]

-- Information about variables defined in the static chain.
type StaticChain = [Int32]

data State = State {
  num_errors :: Integer,
  num_warnings :: Integer,
  next_static_address :: Int32,  -- Next address to assign to static data.
  static :: [(Int32, Static)]    -- Static data currently defined.
}

data SemanticAnalyser a = S (Environment -> StaticChain -> State -> IO (a, State))
