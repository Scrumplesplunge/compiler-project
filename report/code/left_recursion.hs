-- Handle left recursion: this works by turning:
--   parse_a = parse_b
--         ||| parse_a +++ parse_c     >>> f
-- Into:
--   parse_a = left
--               parse_b  -- base case
--               parse_c  -- repeated tail
--               f        -- reducer.
left :: (Eq a, Eq b, Eq c) =>
        Parser a b -> Parser a c -> (b -> c -> b) -> Parser a b
