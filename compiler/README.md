# Occam 1 Compiler

The Occam compiler is written from the ground up and consists of the following
major parts:

  * __Lexer__ - The tokenizer for the Occam syntax.
  * __IndentParser__ - Occam's whitespace-sensitive syntax is dealt with here
    by converting all indentation into __INDENT__ and __DEDENT__ tokens.
  * __Parser__ - The parser for the Occam syntax. This uses the parsing logic
    from __Parsing__ to allow the code for this to bear a resemblence to the BNF
    rules for the grammar.
  * __Semantics__ - The semantic analysis for the compiler is done here, and
    uses the monadic type defined in __SemanticAnalyser__ to allow reasonable
    warning and error messages to be produced at any point during the analysis.
    This takes an abstract syntax tree which is represented as an __AST__, and
    produces one which is represented as an __AnnotatedAST__.
  * __CodeGen__ - The code generation for the compiler.

### Current Status

The compiler can currently generate working code for most of the vital features
of the Occam 1 language. The features which are currently not implemented are:

  * Alternative (ALT) blocks. I am currently trying to work out how to compile
    replicated ALT blocks, before starting to implement this.
  * (Named) Procedures. In theory, there is not much which would have to be done
    to implement these. The static link code that would be needed to reference
    variables outside of the procedure would be the same as that which is used
    by the replicated parallel block. The main focus here will be correctly
    handling the various kinds of arguments that can be passed to procedures.
