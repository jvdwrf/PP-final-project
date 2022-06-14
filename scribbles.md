spawn(t1)




{
let x = 10;
let y = 5;
x + y
};

Prog:
{let f = 0;
let u = f +9;
u};



(1 + 1) * 2;

    Program(
        Statement::Expr(
            Expr::Op {
                expr1: Expr::Parenthesized(
                    Expr::Op {
                        expr1: Expr::Value(1)
                        op: +
                        expr2: Expr::Value(1)
                    }
                )
                op: *
                expr2: Expr::Value(2)
            }
        (
    )

1 + (1 * 2);

    Program(
        Statement::Expr(
            Expr::Op {
                expr1: Expr::Value(1)
                op: +
                expr2: Expr::Parenthesized(
                    Expr::Op {
                        expr1: Expr::Value(1)
                        op: *
                        expr2: Expr::Value(2)
                    }
                )
            }
        (
    )


1 + 2 + 3 * 4 * 4 + 5 < 5 + 3 < 3;

(((((1 + 2) + ((3 * 4) * 4)) + 5) < (5 + 3)) < 3);

(((1 + (2 + ((3 * 4) + 5))) < (5 + 3)) < 3);





[(1+1)*2, "*2"]
1 + 1 + 2;   = (1+1)*2

    Program(
        Statement::Expr(
            Expr::Op {
                expr1: Expr::Value(1)
                op: +
                expr2: Expr::Op {
                    expr1: Expr::Value(1)
                    op: *
                    expr2: Expr::Value(2)
                }
            }
        (
    )

expr = term * expr

## OpLeft
    OpLeft = '+' | '-' | '<' | '<=' | '==' | '>' | '>='

## OpRight
    OpRight = '*'