-- The infix operators allow:

-- a := b
assign :: Parser Token (L Process)
assign = Reduce (\(a, (_, b)) -> L (Assign a b) (location a))
                (Concat expression (Concat (symbol Lexer.ASSIGN) expression))

-- To be rewritten as:

-- a := b
assign :: Parser Token (L Process)
assign = expression +++ symbol Lexer.ASSIGN +++ expression
     >>> (\(a, (_, b)) -> L (Assign a b) (location a))
