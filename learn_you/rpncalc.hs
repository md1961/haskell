import System.IO
import System.Environment
import Data.Char

main = do
    (expression:_) <- getArgs
    let result = calculate expression
    print result

type Operand = Int
type Operator = String
type Term = Either Operand Operator
type Stack = [Operand]

toTerm :: String -> Term
toTerm str
    | isDigit (head str) = Left (read str)
    | otherwise = Right str

calculate :: String -> Operand
calculate expression =
    let terms = map toTerm $ reverse $ words expression
        stack0 = []
        stack = foldr process stack0 terms
        (num, _) = pop stack
    in num

process :: Term -> Stack -> Stack
process (Left num) stack = push num stack
process (Right op) stack = operate op stack

operate :: Operator -> Stack -> Stack
operate op stack =
    let (num2, stack2) = pop stack
        (num1, stack3) = pop stack2
        result = case op of
            "+" -> num1 + num2
            "-" -> num1 - num2
            "*" -> num1 * num2
            "/" -> num1 `div` num2
    in push result stack3

push :: Operand -> Stack -> Stack
push term stack = term:stack

pop :: Stack -> (Operand, Stack)
pop [] = error "Empty stack"
pop stack = (head stack, tail stack)
