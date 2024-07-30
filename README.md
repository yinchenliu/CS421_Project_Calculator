# Calculator
## Overview
The goal of this project was to build a calculator for regular arithmetic expressions entirely from scratch, without using any existing libraries. This involved implementing a parser and evaluator library in Haskell and creating a terminal-based user interface. The project covered several topics learnt from CS421 and other programming language principals, including:
•	Functional Programming 

•	Grammar 

•	LL Parsing 

•	Monadic Features 

•	Stack Testing 

By building the calculator, I gained a deeper understanding of the mechanics and intricacies involved in parser and programming language in general.
## Implementation 
The major tasks of our implementation included:

1.	Defining the Grammar: Designing a grammar for arithmetic expressions that ensures correct precedence and left-associativity.
2.	Implementing the Parser: Building a parser library in Haskell, including the necessary instances for Functor, Applicative, Monad, and Alternative.
3.	Evaluating Expressions: Creating functions to evaluate arithmetic expressions based on the parsed structure.
4.	Building the Command-Line Interface: Developing a user-friendly command-line interface for the calculator, including support for additional features like escape, backspace, delete, and newline handling.

### Grammar 
The grammar used for the arithmetic expressions is designed to handle standard. Initially, the grammar was defined as follows: 
```
•	expr ::= expr + term | expr - term | term 
•	term ::= term * factor | term / factor | factor 
•	factor ::= (expr) | float
```
But to implement the LL parser (top-down parser), we needed to eliminate the left recursion in the expr and term production rules. The modified grammar is:
```
•	expr :: = term expr’ 
•	expr’ :: = + term expr’ | - term expr’ | e 
•	term :: = factor term’ 
•	term’ :: = * factor term’ | / factor term’ | e 
•	factor :: = (expr) | float
```
### Parser
The core of the project is the Parser.hs, where the data type Parser is defined. I implemented instances for Functor, Applicative, Monad, and Alternative for the Parser type. These classes allow us to compose parsers in a modular and expressive way. Additionally, helper functions were defined to parse floats, int, char, string and other basic elements.
### Evaluation
In Eval.hs, I created functions corresponding to each production rule in our grammar, utilizing monadic features to handle sequencing and composition of parsers. The key functions include expr, term, factor, and their respective helper functions (expr' and term’). And finally the eval function will evaluate the parsed expression and return the float output.
### User Interface 
The Main.hs module handles the user interface and integrates the calculator logic. The calc and process functions manage user input and perform calculations. The interface supports additional features such as escape, backspace, delete, and newline handling to enhance user interaction.
### What Works Well 
Parsing: The parser correctly handles arithmetic expressions, including nested expressions and correct precedence. 
Evaluating: The evaluation functions accurately compute the results of parsed expressions. 
UI: The interface is user-friendly and supports additional features like escape, backspace, delete, and newline handling. 
### Partial Implementations 
Error Handling: While basic error handling is implemented, more comprehensive error messages and recovery strategies could be added. 
### Not Implemented 
Extended Features: Additional mathematical functions (e.g., trigonometric, logarithmic) were not implemented but could be future enhancements.
## Tests
### Unit Tests 
For unit testing, I used the HUnit package, a unit testing framework for Haskell. I developed tests for individual parsers to verify their correctness. Examples include tests for parsing single characters, digits, strings, integers, and floats.
### Feature Tests 
Feature tests were written to validate the evaluation of arithmetic expressions. These tests cover simple expressions, expressions with parentheses, and expressions involving floating-point numbers. 
### Larger Tests 
Larger tests were conducted for more complex expressions to ensure correct handling of operator precedence and associativity. These tests included nested expressions and combinations of multiple operations.
All unit tests passed successfully, validating that our parser functions correctly handle various types of input and produce the expected results.
## Listing:
Below are some key functions and data structures from the project, along with explanations and comments.
### Parser:

_Parser_ dataype
```
Parser a = Parser (String-> [(a,String)]
```

The _sat_ function is designed to create a single chafacter if it satisfies a given predicate.
```
sat :: (Char->Bool)->Parser Char
sat pred = do x<- item
              if pred x then return x else empty
```

The _float_ function is defined to parse floating-point numbers from the input. It handles both positive and negative floating-point numbers by utilizing multiple functions.
```
float :: Parser Float
float = do 
    string "-"
    xs<-fat
    return (-xs)
    <|> fat
```


### Evaluation: 

The _expr_ function parses and evaluates arithmetic expressions, handling addition and subtraction.
```
expr :: Parser Float 
expr = do t <- term 
   expr' t
```

The _expr'_ function continues parsing the expression for additional terms that are added or subtracted from the accumulated value. It uses the <|> operator to try parsing either a + or - followed by another term. If neither is found, it returns the accumulated value.
```
expr' :: Float -> Parser Float
expr' acc = (do
  symbol "+"
  t <- term
  expr' (acc + t)) <|> (do
  symbol "-"
  t <- term
  expr' (acc - t)) <|> pure acc
```

The _eval_ function evaluates a given string as an arithmetic expression and returns the result as a Float.
```
eval:: String-> Float
eval xs = case (run expr xs) of
        [(n,[])]-> read (printf "%.4f" n)
        [(_,out)]-> error ("Unused Input "++ out)
        []->error "Invalid Input"
        _->error "Invalid Input"
```
### Main:
The _calc_ function manages the main loop of the command-line calculator, handling user input and updating the display.
```
calc:: String -> IO()
calc xs = do display xs
             c <-getCh
             if elem c buttons then 
                process c xs 
             else 
                do  beep
                    calc xs
```
The _process_ function handles different types of user inputs, determining the appropriate action based on the character entered. It processes commands for quitting, deleting, evaluating, clearing, and handling general input.
```
process :: Char -> String -> IO ()
process c xs 
  | elem c "qQ\ESC" = quit
  | elem c "dD\BS\DEL" = delete xs
  | elem c "=\n" = eval1 xs  -- when "=" or Enter is pressed, evaluate the expression
  | elem c "cC" = clear
  | otherwise = press c xs
```

### Testing:

the testing checks the output with expected results
```
testExprComplexFloat3 :: Test
testExprComplexFloat3 = TestCase (assertEqual "for (eval \"2*3/4-3.5*(2.2-1.4)\")," (-1.3) (eval "2*3/4-3.5*(2.2-1.4)"))
```


## Conclusion
This project successfully demonstrates the implementation of a parser combinator library and an arithmetic expression evaluator in Haskell. Through comprehensive testing and clear documentation, I have ensured that our parser and evaluator handle various expressions correctly and efficiently.

