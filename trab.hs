import Data.List
import Text.ParserCombinators.Parsec

data Type = 
    TypeInt String
    | TypeVar Name
    | TypeArrow Type Type
    deriving Show


type Name = String
type Unifier = [(Name, Type)]

{- instance Show Type where
  show (Atom x) =
    x
  show (TypeVar x) =
    x
  show (TypeArrow x es) =
    x ++ "(" ++ intercalate ", " (map show es) ++ ")" -}

--
-- unit: term eof
--
unit :: Parser Type
unit = do
  t <- parseType
  eof
  return t


main :: IO ()
main = do
  putStrLn "Digite um termo:"
  str <- getLine
  case parse unit "<stdin>" str of
    Right e -> do
      print $ occursCheck "X" e
    Left e ->
      print e



parseType :: Parser Type -- type: function | atom
parseType = do
    try parseAtom -- <|> parseFun

parseAtom :: Parser Type -- atom: int | var | paren
parseAtom = do
    try parseInt <|> parseVar -- <|> parseParen

parseInt :: Parser Type -- int: "Int"
parseInt = do
    show_ <- many1 digit
    return (TypeInt show_)

parseVar :: Parser Type -- var: lowercase+
parseVar = do
  head <- upper
  tail <- many alphaNum
  return (TypeVar (head:tail))


{-
parseFun :: Parser Type -- fun: atom "->" type
parseFun = do
  ret1 <- parseAtom
  ret2 <- ?
  ret3 <- parseType
  return (TypeArrow)


parseParen :: Parser Type -- paren: "(" type ")"
parseParen = do
  char '('
  typ <- parseType
  char ')'

  return ( typ )
-}







{- 
atom :: Parser Type
atom = do
  return (TypeInt many1 lower)


variable :: Parser Type
variable = do
  head <- upper
  tail <- many alphaNum
  return (TypeVar (head:tail))


whitespace :: Parser ()
whitespace = do
  many (char ' ')
  return () -- retornar?


predicate :: Parser Type
predicate = do
  name <- many1 lower
  whitespace
  char '('
  --
  -- sepBy :: Parser a -> Parser b -> Parser [a]
  --
  subterms <- term `sepBy` comma
  char ')'
  return (TypeArrow name subterms)
  where
    -- comma: whitespace "," whitespace
    comma :: Parser ()
    comma = do
      whitespace
      char ','
      whitespace
 -}










unify :: Type -> Type -> Maybe Unifier

unify (TypeInt x) (TypeInt y) | x == y = Just []

--
-- Regra (VAR):
--
--    ---------------- (VAR)
--       X ~ X = {}
--
unify (TypeVar x) (TypeVar y) | x == y =
  -- Existe uma solução, e ela é vazia!
  Just []

--
-- Regra (LEFT):
--        X isn't free in e
--    ------------------------ (LEFT)
--       X ~ e = { X |-> e }
--
unify (TypeVar x) e | not (occursCheck x e) =
  Just [(x, e)]

--
-- Regra (RIGHT):
--        X isn't free in e
--    ------------------------ (RIGHT)
--       e ~ X = { X |-> e }
--
unify e (TypeVar x) | not (occursCheck x e) =
  Just [(x, e)]

unify (TypeInt x) (TypeInt y)
    | x == y = Just []

unify (TypeArrow x xs) (TypeArrow y ys) = 
  case unify x y of
    Just _ -> unify xs ys
    Nothing -> Nothing


unify a b =
  Nothing -- :: Maybe a







--
-- SUBSTITUIÇÃO: recebe um unificador/substituição s
--   e um termo e, e retorna o termo s(e)
--
subst :: Unifier -> Type -> Type

--
-- Átomos fazem parte da estrutura e não mudam; portanto,
--   s(x) = x
--
subst s (TypeInt x) =
  TypeInt x

--
--  A substituição, para variáveis, deve verificar se ela
--  existe dentro do unificador
--    s(X) = e      se s contém X |-> e,
--           X      do contrário
--
subst s (TypeVar x) =
  case lookup x s of
    Just e -> e
    Nothing -> TypeVar x


subst s (TypeArrow x es) =
  -- x :: String
  -- es :: [Type]
  -- Lembrem-se: usamos map pois temos uma LISTA de subtermos!
  TypeArrow x (subst s es)



occursCheck :: Name -> Type -> Bool

occursCheck x (TypeInt y) =
  False

occursCheck x (TypeVar y) =
  x == y

{- occursCheck x (TypeArrow y es) =
  any (occursCheck x) es
 -}


unifyList :: [Type] -> [Type] -> Maybe Unifier
unifyList [] [] = Just []

unifyList (x:xs) (y:ys) = do
    s1 <- unify x y
    s2 <- unifyList (substList s1 xs) (substList s1 ys)
    return (compose s2 s1)

unifyList xs ys = 
    Nothing



substList :: Unifier -> [Type] -> [Type]
substList s xs = 
    map (subst s) xs


substUnifier :: Unifier -> Unifier -> Unifier
substUnifier s xs = 
    map ( \(x,e) -> (x, subst s e) ) xs


compose :: Unifier -> Unifier -> Unifier
compose s2 s1 = 
    s2 ++ substUnifier s2 s1




{-

    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

-}