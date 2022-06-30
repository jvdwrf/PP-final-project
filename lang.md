## Program
A Program that can be parsed

    Program =  ( SharedVars )? Process

## Process

    Process = ( RootStatement )*

## RootStatement
    
    RootStatement =
        | 'spawn' '{' Process '}' ( 'do' '{' Process '}' )?
        | Statement


## SharedVars 

    SharedVars = 'shared' '{' ('let' Identifier '=' Expr ';')* '}'

## Statement
A piece of code that does not return any value.

    Statement =
        | 'let' Identifier '=' Expr ';'                                 // DeclStat
        | 'if' Expr '{' (Statement)* '}' (else '{' (Statement)* '}')?   // IfStat  
        | 'while' Expr '{' (Statement)* '}'                             // WhileStat
        | '{' (Statement)* '}'                                          // BlockStat
        | 'acquire' Identifier '{' (Statement)* '}'                     // AcquireStat
        | Identifier '=' Expr ';'                                       // AssignStat
        | Expr ';'                                                      // ExprStat

## Expr
A piece of code that returns a value.

    Expr = 
        | Expr '<' Expr1        // LtExpr
        | Expr '>' Expr1        // GtExpr
        | Expr '==' Expr1       // EqExpr
        | Expr '!=' Expr1
        | Expr1                 //

    Expr1 = 
        | Expr1 '+' Expr2       // AddExpr
        | Expr1 '-' Expr2       // SubExpr     
        | Expr2                 //

    Expr2 = 
        | Expr2 '*' Expr3       // MulExpr
        | Expr3                 //

    Expr3 = 
        | '(' Expr ')'                  // ParenExpr
        | Method                        // MethodExpr
        | Ident                         // IdentExpr
        | Value                         // ValueExpr

## Method
A built-in method call

    Method 
        = 'print' '(' Expr ')'
        | 'sleep' '(' Expr ')'


## Ident
An identifier, eg a variable name

    Ident = [a..Z] ([a..Z] | [0..9])*

## Value
A value that can be immediately resolved/type-checked. 10 | True

    Value
        = Integer
        | Boolean

## Boolean

    Boolean
        = 'True'
        | 'False'

## Integer

    Integer = ( [0..9] )+

## Whitespace

    Whitespace = 
        | ' '
        | '\n'
        | '//' ( ('\n')! )* '\n'