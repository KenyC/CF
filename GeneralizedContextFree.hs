{-# LANGUAGE RankNTypes #-}


class Applicative f => Collection f where
	union :: f a -> f a -> f a

type ContextFree a = forall f . Collection f => f a

-- When needed to check our languages, I use Lists to represent collections
instance Collection [] where
	union = (++)

-- concatenation
cat :: (Monoid a, Collection f) => f a -> f a -> f a
cat l1 l2 = (pure mappend) <*> l1 <*> l2

-- Kleene star
star :: Monoid a => ContextFree a -> ContextFree a
star l =  (pure mempty) `union` (l `cat` (star l))


type ContextFreeRelation = ContextFree (String, String)

-- establishes a relation between w and ww for any w made of a's and b's
duplicate :: ContextFreeRelation
duplicate  = (pure mempty) `union` ((pure ("a", "a")) `cat` duplicate `cat` (pure ("", "a"))) `union`
									((pure ("b", "b")) `cat` duplicate `cat` (pure ("", "b")))



class BinaryTree a where
	merge :: a -> a -> a
	left :: a -> Maybe a  
	right :: a -> Maybe a  

lMerge :: BinaryTree a => ContextFree a -> ContextFree a -> ContextFree a
lMerge l1 l2 = (pure merge) <*> l1 <*> l2

lLeft :: BinaryTree a => ContextFree a -> ContextFree (Maybe a)
lLeft = fmap left

lRight :: BinaryTree a => ContextFree a -> ContextFree (Maybe a)
lRight = fmap right
