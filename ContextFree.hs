{-# LANGUAGE RankNTypes #-}
import qualified Data.Set as Set



class Applicative f => Collection f where
	union :: f a -> f a -> f a


instance Collection [] where
	union = (++)


type Language = forall f . Collection f => f String

cat :: Language -> Language -> Language
cat l1 l2 = ((pure (++)) <*> l1) <*> l2


-- Examples

-- language1 : ab + ba
language1 :: Language
language1 = ((pure "a") `cat` (pure "b")) `union` ((pure "b") `cat` (pure "a"))

-- language2 : ab(a+b)
language2 :: Language
language2 = (pure "a") `cat` (pure "b") `cat` ((pure "a") `union` (pure "b"))

-- language3: a*
language3 :: Language
language3 = (pure "") `union` ((pure "a") `cat` language3)

-- Kleene start \L -> L*
star :: Language -> Language
star l = (pure "") `union` (l `cat` (star l))

-- language4: a*b*
language4 :: Language
language4 = (star $ pure "a") `cat` (star $ pure "b")

-- language5: {a^nb^n | n >= 0}
language5 :: Language
language5 = pure "" `union` (pure "a" `cat` language5 `cat` pure "b")

{-
Grammar:
TUPLE -> ( VALUES )
VALUES -> VALUES, TUPLE
VALUES -> empty string
-}
tuple :: Language
values :: Language
tuple = (pure "(") `cat` values `cat` (pure ")")
values = (pure "") `union` (values `cat` pure "," `cat` tuple)

-- defining recursivity in terms of fixed point
fix :: (a -> a) -> a
fix f = f $ fix f

-- language3prime: a*
rec_l3 :: Language -> Language
rec_l3 l = (pure "") `union` ((pure "a") `cat` l)
language3prime :: Language
language3prime = fix rec
