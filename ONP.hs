-- spoj ONP
-- 中缀表达式转后缀表达式, 用两个栈做的,
-- 注意如果当前操作符优先级较小的时候需要递归下去.
-- 另外, 操作符之间的优先关系是不满足Ord的, 不能简单的用数值代替.

import Data.Char
import Data.List
import Data.Map (fromList, (!))

data Op = Op Char deriving(Show)
data Expr = Var Char | Arith Expr Op Expr deriving(Show)

comp (Op a) (Op b) = mp ! (a, b)
    where mp = fromList $ zip [(a, b) | a <- ops, b <- ops] table
          ops = "+-*/()$^"
          table = [
            '>','>','<','<','<','>','>','<',
            '>','>','<','<','<','>','>','<',
            '>','>','>','>','<','>','>','<',
            '>','>','>','>','<','>','>','<',
            '<','<','<','<','<','=','?','<',
            '>','>','>','>','?','>','>','>',
            '<','<','<','<','<','?','=','<',
            '>','>','>','>','<','>','>','>'
            ]

readIFN :: String -> Expr
readIFN = head . fst . foldl f ([], [Op '$']) . (++"$")
    where f stk@(exprs, ops) ch
            | isAlpha ch = (Var ch:exprs, ops)
            | comp (head ops) (Op ch) == '<' = (exprs, Op ch:ops)
            | comp (head ops) (Op ch) == '=' = (exprs, tail ops)
            | otherwise = f (g stk, tail ops) ch
            where g (rhs:lhs:exprs, op:ops) = Arith lhs op rhs:exprs

showRPN (Var ch) = [ch]
showRPN (Arith lhs (Op op) rhs) = showRPN lhs ++ showRPN rhs ++ [op]

main = do
    input <- getLine
    let testcount = read input
    sequence . take testcount . repeat $ do
        input <- getLine
        putStrLn $ showRPN $ readIFN input
        --putStrLn $ show $ readIFN input
