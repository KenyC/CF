{-# LANGUAGE RankNTypes #-}
import qualified Data.Set as Set



class Applicative f => Collection f where
	union :: f a -> f a -> f a


instance Collection [] where
	union = (++)
-- use "show $ take 10 l" to display elements from any given language "l"
-- TODO: in practice, we'd be better off with something that interspersed elements from both lists ; otherwise, listing elements form the language is not exhaustive
type ContextFree = forall f . Collection f => f String

cat :: ContextFree -> ContextFree -> ContextFree
cat l1 l2 = (pure (++)) <*> l1 <*> l2


-- Examples

-- language1 : ab + ba
language1 :: ContextFree
language1 = ((pure "a") `cat` (pure "b")) `union` ((pure "b") `cat` (pure "a"))

-- language2 : ab(a+b)
language2 :: ContextFree
language2 = (pure "a") `cat` (pure "b") `cat` ((pure "a") `union` (pure "b"))

-- language3: a*
language3 :: ContextFree
language3 = (pure "") `union` ((pure "a") `cat` language3)

-- Kleene start \L -> L*
star :: ContextFree -> ContextFree
star l = pure "" `union` (l `cat` (star l))

-- language4: a*b*
language4 :: ContextFree
language4 = (star $ pure "a") `cat` (star $ pure "b")

-- language5: {a^nb^n | n >= 0}
language5 :: ContextFree
language5 = pure "" `union` (pure "a" `cat` language5 `cat` pure "b")

{-
Grammar:
TUPLE -> ( VALUES )
VALUES -> VALUES, TUPLE
VALUES -> empty string
-}
tuple :: ContextFree
values :: ContextFree
tuple = (pure "(") `cat` values `cat` (pure ")")
values = (pure "") `union` (values `cat` pure "," `cat` tuple)

-- defining recursivity in terms of fixed point
fix :: (ContextFree -> ContextFree) -> ContextFree
fix f = f $ fix f

-- language3prime: a*
rec_l3 :: ContextFree -> ContextFree
rec_l3 l = (pure "") `union` ((pure "a") `cat` l)
language3prime :: ContextFree
language3prime = fix rec_l3


-- context-sensitive languages, if we allow "non-primitive" lifted operations
-- language6 : a^nba^nba^n
language6 :: ContextFree
language6 = fmap (\x -> x ++ "b" ++ x ++ "b" ++ x) (star $ pure "a")