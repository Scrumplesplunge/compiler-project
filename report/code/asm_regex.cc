// Instruction pattern.
const char pattern_string[] =
  //     12          2 1
  R"(^\s*(([A-Z0-9_]+):)?)"                    // Line label (if any)
    //    34         45   6
    R"(\s*(([a-z0-9]+)(\s+()"                  // Mnemonic, optional:
      R"(-?0x[0-9a-fA-F]+|)"                   // Hexadecimal argument
      R"(-?[1-9][0-9]*|)"                      // Decimal argument
      R"(-?[0-7]+|)"                           // Octal argument
      // 7          78       9          98
      R"(([A-Z0-9_]+)(\s*-\s*([A-Z0-9_]+))?)"  // Symbolic argument
    // 65 3
    R"())?)?)"                                 // end of optional
  //    A   A
  R"(\s*(#.*)?$)";                             // Comment
