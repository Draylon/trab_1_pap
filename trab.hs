import Data.List
import Text.ParserCombinators.Parsec

data Type = 
    TypeInt
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


{- main :: IO ()
main = do
  putStrLn "Digite um termo:"
  str <- getLine
  case parse unit "<stdin>" str of
    Right e -> do
      print $ occursCheck "X" e
    Left e ->
      print e -}


main :: IO ()
main = do
  putStrLn "Digite um termo:"
  str1 <- getLine
  let Right a = parse unit "<stdin>" str1
  str2 <- getLine
  let Right b = parse unit "<stdin>" str2
  --
  putStrLn "Unificacao:"
  case unify a b of
    Just s ->
      putStrLn $ showUnifier s
    Nothing ->
      putStrLn "Deu ruim!"


showUnifier [] =
  "{}"
showUnifier xs =
  "{ " ++ intercalate ", " (map showPair xs) ++ " }"
  where
    showPair (x, e) =
      x ++ " |-> " ++ show e
      

whitespace :: Parser ()
whitespace = do
  -- Note que não usamos o many1!
  many (char ' ')
  return ()



parseType :: Parser Type -- type: function | atom
parseType =
    try parseFun <|> parseAtom

parseAtom :: Parser Type -- atom: int | var | paren
parseAtom =
    try parseInt <|> parseVar <|> parseParen

parseInt :: Parser Type -- int: "Int"
parseInt = do
    string "Int"
    return (TypeInt)

parseVar :: Parser Type -- var: lowercase+
parseVar = do
  name <- many1 lower
  return (TypeVar name)



parseFun :: Parser Type -- fun: atom "->" type
parseFun = do
  ret1 <- parseAtom
  whitespace
  string "->"
  whitespace
  ret3 <- parseType
  return (TypeArrow ret1 ret3)


parseParen :: Parser Type -- paren: "(" type ")"
parseParen = do
  char '('
  whitespace
  typ <- parseType
  whitespace
  char ')'

  return ( typ )







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

unify (TypeInt) (TypeInt) = Just []

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
unify (TypeVar x) e 
  | not (occursCheck x e) = Just [(x, e)]

--
-- Regra (RIGHT):
--        X isn't free in e
--    ------------------------ (RIGHT)
--       e ~ X = { X |-> e }
--
unify e (TypeVar x)
  | not (occursCheck x e) = Just [(x, e)]



unify (TypeArrow x xs) (TypeArrow y ys) = do
    s1 <- unify x y
    s2 <- unify (subst s1 xs) (subst s1 ys)
    return (compose s2 s1)


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
subst s (TypeInt) =
  TypeInt

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


subst s (TypeArrow x y) =
  TypeArrow x (subst s y)



occursCheck :: Name -> Type -> Bool

occursCheck x (TypeInt) =
  False

occursCheck x (TypeVar y) =
  x == y

occursCheck x (TypeArrow y es) =
  occursCheck x es



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