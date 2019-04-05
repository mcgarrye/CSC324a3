module A4 where

import           Control.Applicative
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Control.Monad
import           A4Def
import           ParserLib

-- This can help testing by reading from a file so you can test multi-line input
-- and also have little hassle with \
parseFile :: String -> IO (Maybe Expr)
parseFile filename = do
    inp <- readFile filename
    let ans = runParser mainParser inp
    return ans

mainParser :: Parser Expr
mainParser = whitespaces *> block <* eof
    where 
        block = cond <|> lambda <|> local <|> infixx
        infixx = chainr1 arith cmp
        cmp = (string "==" *> whitespaces *> pure (Prim2 Eq)) <|> (char '<' *> whitespaces *> pure (Prim2 Lt))
        arith = chainl1 addend addop
        addop = (char '+' *> whitespaces *> pure (Prim2 Plus)) <|> (char '-' *> whitespaces *> pure (Prim2 Minus))
        addend = chainl1 factor mulop
        mulop = (char '*' *> whitespaces *> pure (Prim2 Mul)) <|> (char '/' *> whitespaces *> pure (Prim2 Div)) <|> (char '%' *> whitespaces *> pure (Prim2 Mod))
        factor = do
            left <- factor <|> atom
            right <- atom
            pure (App left right)
        atom = (between (char '(' *> whitespaces) (char ')' *> whitespaces) block) <|> literal <|> (fmap Var var)
        cond = do
            keyword "if" *> whitespaces
            b_if <- block
            keyword "then" *> whitespaces
            b_then <- block
            keyword "else" *> whitespaces
            b_else <- block
            pure (Cond b_if b_then b_else)
        lambda = do
            keyword "\\" *> whitespaces
            s <- var
            keyword "->" *> whitespaces
            b <- block
            pure (Lambda s b)
        local = do
            keyword "let" *> whitespaces
            eqns <- many equation
            keyword "in" *> whitespaces
            e <- block
            pure (Let eqns e)
        equation = do
            v <- var
            char '=' *> whitespaces
            e <- block
            char ';' *> whitespaces
            pure (v, e)
        literal = (fmap Num integer) <|> boolean
        var = identifier ["if", "then","else", "let", "in", "True", "False"]
        boolean = (string "True" *> whitespaces *> pure (Bln True)) <|> (string "False" *> whitespaces *> pure (Bln False))


mainInterp :: Expr -> Either Error Value
mainInterp = interp Map.empty

intOrDie :: Value -> Either Error Integer
intOrDie (VN i) = pure i
intOrDie _ = Left TypeError

interp :: Map String Value -> Expr -> Either Error Value
interp _ (Num i) = pure (VN i)

interp _ (Bln b) = pure (VB b)

interp env (Var v) = case Map.lookup v env of
  Just a -> pure a
  Nothing -> Left VarNotFound

interp env (Prim2 Plus e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VN (i + j))

interp env (Prim2 Minus e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VN (i - j))

interp env (Prim2 Mul e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VN (i * j))

interp env (Prim2 Div e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    case j of
        0 -> Left DivByZero
        _ -> pure (VN (div i j))

interp env (Prim2 Mod e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    case j of
        0 -> Left DivByZero
        _ -> pure (VN (mod i j))

interp env (Prim2 Eq e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VB (i == j))

interp env (Prim2 Lt e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VB (i < j))

interp env (Cond test eThen eElse) = do
    a <- interp env test
    case a of
      VB True -> interp env eThen
      VB False -> interp env eElse
      _ -> Left TypeError

interp env (Let eqns evalMe) = do
    env' <- extend env eqns
    interp env' evalMe
  where
    extend = foldM extendOnce
    extendOnce env (v,rhs) = do
        a <- interp env rhs
        pure (Map.insert v a env)

interp env (Lambda v body) = pure (VClosure env v body)

interp env (App f e) = do
    c <- interp env f
    case c of
      VClosure fEnv v body -> do
          eVal <- interp env e
          let bEnv = Map.insert v eVal fEnv  -- fEnv, not env
          interp bEnv body
      _ -> Left TypeError
