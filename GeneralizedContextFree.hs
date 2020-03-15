{-# LANGUAGE RankNTypes #-}


class Applicative f => Collection f where
	union :: f a -> f a -> f a



type ContextFree a = Monoid a => forall f . Collection f => f a

concatenate :: (Monoid a, Collection f) => f a -> f a -> f a
concatenate l1 l2 = (pure mappend) <*> l1 <*> l2


star :: Monoid a => ContextFree a -> ContextFree a
star l =  (pure mempty) `union` (l `concatenate` (star l))

-- When needed to check our languages, I use Lists to represent collections
instance Collection [] where
	union = (++)

-- Claim:
-- the context-free languages are the languages that can be defined in the lazy calculous of Haskell


-- Examples

-- Given a parenthesis construct the parenthesis language with that parenthesis  
dyck_1 :: Monoid a => a -> a -> ContextFree a
dyck_1 a b = (pure mempty)  `union` ((pure a) `concatenate` (dyck_1 a b)  `concatenate` (pure b)) `union` ((dyck_1 a b) `concatenate` (dyck_1 a b))


-- To test that our languages are working, we'll instantiate the type List
test :: [String]
test = dyck_1 "a" "b"
-- not all elements of the parenthesis language will be available through this
