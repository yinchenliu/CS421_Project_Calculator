Calculator

Overview

The goal of this project was to build a calculator for regular arithmetic expressions entirely from scratch, without using any existing libraries. This involved implementing a parser and evaluator library in Haskell and creating a terminal based user interface. The project covered several topics learnt from CS421 and other programming language principals, including:
Functional Programming
Grammar
LL Parsing
Monadic Features
Stack Testing
By building the calculator, I gained a deeper understanding of the mechanics and intricacies involved in parser and programming language in general.

Implementation
Grammar
The grammar used for the arithmetic expressions is designed to handle standard operations with correct precedence and left-associativity. Initially, the grammar was defined as follows:
expr ::= expr + term | expr - term | term 
term ::= term * factor | term / factor | factor 
factor ::= (expr) | float

But to implement the LL parser (top-down parser), we needed to eliminate the left recursion in the expr and term production rules. The modified grammar is: 

expr :: = term expr’
expr’ :: = + term expr’ | - term expr’ | e
term :: = factor term’
term’ :: = * factor term’ | / factor term’ | e
factor :: = (expr) | float

Parser.hs

The core of the project is the parser.hs, where the data type Parser is defined.
I implemented instances for Functor, Applicative, Monad, and Alternative for the Parser type. These classes allow us to compose parsers in a modular and expressive way. Additionally, helper functions were defined to parse floats, int, char, string and other basic elements.

Eval.hs

In Eval.hs, I created functions corresponding to each production rule in our grammar, utilizing monadic features to handle sequencing and composition of parsers. The key functions include expr, term, factor, and their respective helper functions (expr' and term’).
And finally the eval function will evaluate the parsed expression and return the float output. 

Main.hs
The Main.hs module handles the user interface and integrates the calculator logic. The calc and process functions manage user input and perform calculations. The interface supports additional features such as escape, backspace, delete, and newline handling to enhance user interaction.

What Works Well
Parsing: The parser correctly handles arithmetic expressions, including nested expressions and correct precedence.
Evaluation: The evaluation functions accurately compute the results of parsed expressions.
Terminal Interface: The interface is user-friendly and supports additional features like escape, backspace, delete, and newline handling.
Partial Implementations
Error Handling: While basic error handling is implemented, more comprehensive error messages and recovery strategies could be added.
Not Implemented
Extended Features: Additional mathematical functions (e.g., trigonometric, logarithmic) were not implemented but could be future enhancements.

Tests

Unit Tests
For unit testing, I used the HUnit package, a unit testing framework for Haskell. I developed tests for individual parsers to verify their correctness. Examples include tests for parsing single characters, digits, strings, integers, and floats.

Feature Tests
Feature tests were written to validate the evaluation of arithmetic expressions. These tests cover simple expressions, expressions with parentheses, and expressions involving floating-point numbers.
Larger Tests
Larger tests were conducted for more complex expressions to ensure correct handling of operator precedence and associativity. These tests included nested expressions and combinations of multiple operations.

All tests passed.

Listing:



