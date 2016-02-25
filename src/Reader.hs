module Reader where

import Core
import Text.Parsec
import Text.Parsec.Char

type Parser = Parsec String Bool

lispInt :: Parser Atom
lispInt = do
    digits <- many1 digit
    return $ Int (read digits :: Int)

lispBoolean :: Parser Atom
lispBoolean =
    (string "true" >> (return $ Bool True)) <|>
    (string "false" >> (return $ Bool False))

symbolSpecialChars = char '*' <|> char '_' <|> char '-' <|> char '$'

lispSymbol :: Parser Atom
lispSymbol = do
    x <- letter <|> symbolSpecialChars
    xs <- many $ alphaNum <|> symbolSpecialChars
    return $ Symbol $ Sym (x : xs)

lispAtom :: Parser Form
lispAtom = fmap Atom (lispBoolean <|> lispInt <|> lispSymbol)

quote form = (Atom $ Symbol $ Sym "quote") : form

inParens = between (char '(') (char ')')

lispSexp :: Parser Form
lispSexp = do
    quo <- option False (char '\'' >> return True)
    forms <- inParens $ lisp `sepBy` spaces
    return $
        SExp $
        if quo
            then (quote forms)
            else forms

lisp = lispAtom <|> lispSexp
