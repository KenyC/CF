import Prelude hiding (Rational)

 
class Rational f where

	lift :: a -> f a
	union :: f a -> f a -> f a
	times :: Monoid a => f a -> f a -> f a

(<+>) :: Rational f => f a -> f a -> f a
(<+>) = union
(<.>) :: (Rational f, Monoid a) => f a -> f a -> f a
(<.>) = times

star :: (Rational f, Monoid a) => f a -> f a
star l =  (lift neuter) <+> (l <.> (star l))


instance Rational [] where
	lift = return
	union = (++)
	times l l' = do
						w <- l
						w' <- l'
						return $ w <> w' 

test :: [String]
test = (star $ lift "a") <.> (star $ lift "b")