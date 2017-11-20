module Tree where

import           Data.Monoid

data Tree a = Node a (Tree a) (Tree a)
            | Empty
    deriving Show

example0 = Empty

example1 = Node 'a' Empty Empty

example2 = Node 'a' (Node 'b' Empty Empty) Empty

singleton :: a -> Tree a
singleton x = Node x Empty Empty

instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Node a subL subR) = foldMap f subL <> f a <> foldMap f subR
