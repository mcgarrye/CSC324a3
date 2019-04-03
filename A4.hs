module A4 where

import           Control.Applicative
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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
		factor = chainl1 atom atom
		atom = between (char '(' *> whitespaces) (char ')' *> whitespaces) block <|> literal <|> (fmap Var var)
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
mainInterp = error "TODO"
