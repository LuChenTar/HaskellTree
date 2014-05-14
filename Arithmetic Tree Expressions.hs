module Tree_Expressions (
 Expression (Number, Node),
 Operand (Plus, Minus, Times, Divided_By, Power),
 eval -- :: Expression -> Expression
) where

data Expression a = Number a | Node (Expression a) Operand (Expression a)
    deriving (Show, Eq)
data Operand = Plus | Minus | Times | Divided_By | Power
    deriving (Show, Eq)

eval :: Floating a => Expression a -> Expression a
eval inp = case inp of
    Number n -> Number n
    Node expressionl operation expressionr -> case (expressionl, operation, expressionr) of
        (Number a, operation, Number b) ->  calculator (Number a) (Number b) operation
            where
                calculator :: Floating a => Expression a -> Expression a -> Operand -> Expression a
                calculator (Number x) (Number y) operate = case operate of
                    Plus -> Number (x + y)
                    Minus -> Number (x - y)
                    Times -> Number (x * y)
                    Divided_By -> Number (x / y)
                    Power -> Number (x ** y)
        (expressionl, operation, expressionr) -> eval (Node (eval expressionl) operation (eval expressionr))
