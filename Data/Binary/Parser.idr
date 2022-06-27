||| A simple parser combinator library for (Vect n Int). Inspired by attoparsec zepto.
module Data.Binary.Parser
import public Control.Monad.Identity
import Control.Monad.Trans

import Data.String
import Data.String.Extra
import Data.Fin
import Data.List
import Data.List.Alternating
import Data.List1
import Data.SnocList
import Data.Vect

%default total

||| The input state, pos is position in the string and maxPos is the length of the input string.
public export
record State where
    constructor S
    maxLen : Nat
    input : Vect maxLen Int
    pos : Nat

Show State where
    show s = "(" ++ show s.input ++ ", " ++ show s.pos ++ ")"

||| Result of applying a parser
public export
data Result a = Fail Nat String | OK a State

Functor Result where
    map f (Fail i err) = Fail i err
    map f (OK r s)     = OK (f r) s

public export
record ParseT (m : Type -> Type) (a : Type) where
    constructor P
    runParser : State -> m (Result a)

public export
Parser : Type -> Type
Parser = ParseT Identity

public export
Functor m => Functor (ParseT m) where
    map f p = P $ \s => map (map f) (p.runParser s)

public export
Monad m => Applicative (ParseT m) where
    pure x = P $ \s => pure $ OK x s
    f <*> x = P $ \s => case !(f.runParser s) of
                            OK f' s' => map (map f') (x.runParser s')
                            Fail i err => pure $ Fail i err

public export
Monad m => Alternative (ParseT m) where
    empty = P $ \s => pure $ Fail s.pos "no alternative left"
    a <|> b = P $ \s => case !(a.runParser s) of
                            OK r s' => pure $ OK r s'
                            Fail _ _ => b.runParser s

public export
Monad m => Monad (ParseT m) where
    m >>= k = P $ \s => case !(m.runParser s) of
                             OK a s' => (k a).runParser s'
                             Fail i err => pure $ Fail i err

public export
MonadTrans ParseT where
    lift x = P $ \s => map (flip OK s) x

||| Run a parser in a monad
||| Returns a tuple of the result and final position on success.
||| Returns an error message on failure.
export
parseT : {n : Nat} -> Functor m => ParseT m a -> Vect n Int -> m (Either String (a, Nat))
parseT p is = map (\case
                       OK r s     => Right (r, s.pos)
                       Fail i err => Left $ "Parse failed at position [\{show i}/\{show $ length is}]: \{err}")
                   (p.runParser $ S n is 0)

||| Run a parser in a pure function
||| Returns a tuple of the result and final position on success.
||| Returns an error message on failure.
export
parse : {n : Nat} -> Parser a -> Vect n Int -> Either String (a, Nat)
parse p is = runIdentity $ parseT p is

||| Combinator that replaces the error message on failure.
||| This allows combinators to output relevant errors
export
(<?>) : Functor m => ParseT m a -> String -> ParseT m a
(<?>) p msg = P $ \s => map (\case
                                OK r s'  => OK r s'
                                Fail i _ => Fail i msg)
                            (p.runParser s)

infixl 0 <?>

||| Combinator that combines the error message on failure with another.
||| This allows combinators to output relevant errors similar to a stack trace.
export
(<?+>) : Functor m => ParseT m a -> String -> ParseT m a
(<?+>) p m2 = P $ \s => map (\case
                                OK r s'   => OK r s'
                                Fail i m1 => Fail i $ m2 ++ ":\n\t" ++ m1)
                            (p.runParser s)
infixl 0 <?+>

||| Discards the result of a parser
export
skip : Functor m => ParseT m a -> ParseT m ()
skip = ignore

||| Maps the result of the parser `p` or returns `def` if it fails.
export
optionMap : Functor m => b -> (a -> b) -> ParseT m a -> ParseT m b
optionMap def f p = P $ \s => map (\case
                                     OK r s'  => OK (f r) s'
                                     Fail _ _ => OK def s)
                                  (p.runParser s)

||| Runs the result of the parser `p` or returns `def` if it fails.
export
option : Functor m => a -> ParseT m a -> ParseT m a
option def = optionMap def id

||| Returns a Bool indicating whether `p` succeeded
export
succeeds : Functor m => ParseT m a -> ParseT m Bool
succeeds = optionMap False (const True)

||| Returns a Maybe that contains the result of `p` if it succeeds or `Nothing` if it fails.
export
optional : Functor m => ParseT m a -> ParseT m (Maybe a)
optional = optionMap Nothing Just

||| Succeeds if and only if the argument parser fails.
|||
||| In Parsec, this combinator is called `notFollowedBy`.
export
requireFailure : Functor m => ParseT m a -> ParseT m ()
requireFailure (P runParser) = P $ \s => reverseResult s <$> runParser s
where
  reverseResult : State -> Result a -> Result ()
  reverseResult s (Fail _ _) = OK () s
  reverseResult s (OK _ _)   = Fail (pos s) "purposefully changed OK to Fail"

||| Fail with some error message
export
fail : Applicative m => String -> ParseT m a
fail x = P $ \s => pure $ Fail s.pos x

||| Succeeds if the next value satisfies the predicate `f`
export
satisfy : Applicative m => (Int -> Bool) -> ParseT m Int
satisfy f = P $ \s => pure $ case natToFin s.pos s.maxLen of
                                 Just p => let i = index p s.input in
                                               if f i
                                                   then OK i (S s.maxLen s.input $ s.pos + 1)
                                                   else Fail s.pos "could not satisfy predicate"
                                 Nothing => Fail s.pos "could not satisfy predicate"

||| `satisfy`, but accepts a Char instead of an Int value.
satisfyChar : Applicative m => (Char -> Bool) -> ParseT m Char
satisfyChar f = map cast $ satisfy $ f . cast

||| Match a single value
export
match : Applicative m => Int -> ParseT m Int
match v = P $ \s => pure $ case natToFin s.pos s.maxLen of
    Nothing => Fail s.pos "somehow read past maxLen! Report this to the library maintainers!"  -- TODO: handle better
    Just p  => let x = index p s.input
               in if x == v
                      then OK v (S s.maxLen s.input (s.pos + 1))
                      else Fail s.pos ("expected \{show v}, got \{show x}")

||| Succeeds if the list of values `is` follows.
export
matchList : Applicative m => List Int -> ParseT m (List Int)
matchList is = P $ \s => pure $ let len = length is in
                              if s.pos+len <= s.maxLen
                                  then let subv = drop s.pos is in
                                       if subv `isPrefixOf` is
                                         then OK is (S s.maxLen s.input (s.pos + len))
                                         else Fail s.pos ("expected \{show is}, got \{show subv}")
                                  else Fail s.pos ("reached end of input while searching for \{show is}")

||| Succeeds if the end of the input is reached.
export
eos : Applicative m => ParseT m ()
eos = P $ \s => pure $ if s.pos == s.maxLen
                           then OK () s
                           else Fail s.pos "expected the end of the input"

||| Succeeds if the next char is `c`
export
char : Applicative m => Char -> ParseT m Char
char c = satisfyChar (== c) <?> "expected \{show c}"

||| Succeeds if the string `str` follows.
export
string : Applicative m => String -> ParseT m String
string = map (fastPack . map cast) . matchList . map cast . fastUnpack

||| Parses a space character
export
space : Applicative m => ParseT m Char
space = satisfyChar isSpace <?> "expected space"

||| Parses a letter or digit (a character between \'0\' and \'9\').
||| Returns the parsed character.
export
alphaNum : Applicative m => ParseT m Char
alphaNum = satisfyChar isAlphaNum <?> "expected letter or digit"

||| Parses a letter (an upper case or lower case character). Returns the
||| parsed character.
export
letter : Applicative m => ParseT m Char
letter = satisfyChar isAlpha <?> "expected letter"

-- TODO: may be possible to make these total since we use vectors?
mutual
    ||| Succeeds if `p` succeeds, will continue to match `p` until it fails
    ||| and accumulate the results in a list
    export
    covering
    some : Monad m => ParseT m a -> ParseT m (List a)
    some p = [| p :: many p |]

    ||| Always succeeds, will accumulate the results of `p` in a list until it fails.
    export
    covering
    many : Monad m => ParseT m a -> ParseT m (List a)
    many p = some p <|> pure []

||| Parse left-nested lists of the form `((start op arg) op arg) op arg`
export
covering
hchainl : Monad m => ParseT m start -> ParseT m (start -> arg -> start) -> ParseT m arg -> ParseT m start
hchainl pst pop parg = pst >>= go
  where
  covering
  go : start -> ParseT m start
  go x = (do op <- pop
             arg <- parg
             go $ op x arg) <|> pure x

||| Parse right-nested lists of the form `arg op (arg op (arg op end))`
export
covering
hchainr : Monad m => ParseT m arg -> ParseT m (arg -> end -> end) -> ParseT m end -> ParseT m end
hchainr parg pop pend = go id <*> pend
  where
  covering
  go : (end -> end) -> ParseT m (end -> end)
  go f = (do arg <- parg
             op <- pop
             go $ f . op arg) <|> pure f

||| Always succeeds, applies the predicate `f` on chars until it fails and creates a string
||| from the results.
export
covering
takeWhile : Monad m => (Int -> Bool) -> ParseT m (List Int)
takeWhile f = many (satisfy f)

||| Similar to `takeWhile` but fails if the resulting string is empty.
export
covering
takeWhile1 : Monad m => (Int -> Bool) -> ParseT m (List Int)
takeWhile1 f = some (satisfy f)

||| Takes from the input until the `stop` string is found.
||| Fails if the `stop` string cannot be found.
export
covering
takeUntil : Monad m => (stop : List Int) -> ParseT m (List Int)
takeUntil stop = do
    case stop of
      []         => pure []
      (s :: top) => takeUntil' s top [<]
  where
    takeUntil' : Monad m' => (s : Int) -> (top : List Int) -> (acc : SnocList (List Int)) -> ParseT m' (List Int)
    takeUntil' s top acc = do
        init <- takeWhile (/= s)
        skip $ match s <?> "end of string reached - \{show stop} not found"
        case !(succeeds $ matchList top) of
             False => takeUntil' s top $ acc :< (init `snoc` s)
             True => pure $ concat $ acc :< init

||| Parses zero or more space characters
export
covering
spaces : Monad m => ParseT m ()
spaces = skip (many space)

||| Parses one or more space characters
export
covering
spaces1 : Monad m => ParseT m ()
spaces1 = skip (some space) <?> "whitespaces"

||| Discards brackets around a matching parser
export
parens : Monad m => ParseT m a -> ParseT m a
parens p = char '(' *> p <* char ')'

||| Discards whitespace after a matching parser
export
covering
lexeme : Monad m => ParseT m a -> ParseT m a
lexeme p = p <* spaces

||| Matches a specific string, then skips following whitespace
export
covering
token : Monad m => String -> ParseT m ()
token s = lexeme (skip $ string s) <?> "expected token " ++ show s

||| Parse repeated instances of at least one `p`, separated by `s`,
||| returning a list of successes.
|||
||| @ p the parser for items
||| @ s the parser for separators
export
covering
sepBy1 : Monad m => (p : ParseT m a)
                 -> (s : ParseT m b)
                 -> ParseT m (List1 a)
sepBy1 p s = [| p ::: many (s *> p) |]

||| Parse zero or more `p`s, separated by `s`s, returning a list of
||| successes.
|||
||| @ p the parser for items
||| @ s the parser for separators
export
covering
sepBy : Monad m => (p : ParseT m a)
                -> (s : ParseT m b)
                -> ParseT m (List a)
sepBy p s = optionMap [] forget (p `sepBy1` s)

||| Parses /one/ or more occurrences of `p` separated by `comma`.
export
covering
commaSep1 : Monad m => ParseT m a -> ParseT m (List1 a)
commaSep1 p = p `sepBy1` (char ',')

||| Parses /zero/ or more occurrences of `p` separated by `comma`.
export
covering
commaSep : Monad m => ParseT m a -> ParseT m (List a)
commaSep p = p `sepBy` (char ',')

||| Parses alternating occurrences of `a`s and `b`s.
||| Can be thought of as parsing:
||| - a list of `b`s, separated, and surrounded, by `a`s
||| - a non-empty list of `a`s, separated by `b`s
||| where we care about the separators
export
covering
alternating : Monad m
           => ParseT m a
           -> ParseT m b
           -> ParseT m (Odd a b)
alternating x y = do vx <- x
                     (vx ::) <$> [| y :: alternating x y |] <|> pure [vx]

||| Run the specified parser precisely `n` times, returning a vector
||| of successes.
export
ntimes : Monad m => (n : Nat) -> ParseT m a -> ParseT m (Vect n a)
ntimes    Z  p = pure Vect.Nil
ntimes (S n) p = [| p :: (ntimes n p) |]
