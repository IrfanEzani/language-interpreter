Implementation of OCaml files to interpret a subset of the CAML language, called MicroCAML. Each file has a specific role in the interpretation process:

## microCaml_evaluator.ml

Key Features:
- Environment handling (variable bindings, lookup functions)
- Expression evaluation (arithmetic, boolean, comparisons)
- Error management (type errors, unbound variables, divide by zero)

## microCaml_lexer.ml

Key Features:
- Regular expression-based tokenization
- Handling various token types (parentheses, operators, comparisons, etc.)

## microCaml_parser.ml

Primary Functions:
- AST construction
- Syntax validation
- Error handling for unexpected tokens

Integration of all forms a basic interpreter for MicroCAML, understanding the basics of language interpretation.

