{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module ClassInheritanceBasic where

data Tree a
  = Node (Tree a) (Tree a)
  | Leaf a
  deriving (Show, Eq)

data Sum = Sum deriving Show
data Plus3 = Plus3 deriving Show
data CoolNode = CoolNode deriving Show
data Plus5 = Plus5 deriving Show
data Base = Base deriving Show
data Succ = Succ deriving Show

class TreeCataNode a r current target | a current target -> r where
  cataNode :: current -> target -> Tree a -> r

class TreeCataLeaf a r current target | a current target -> r where
  cataLeaf :: current -> target -> Tree a -> r

class (TreeCataNode a r current target, TreeCataLeaf a r current target) => TreeCata a r current target | a current target -> r where
  cata' :: current -> target -> Tree a -> r
  cata' current target t@(Node _ _) = cataNode current target t
  cata' current target t@(Leaf _) = cataLeaf current target t

cata :: TreeCata a r target target => target -> Tree a -> r
cata target = cata' target target

-- Some magic to call base method if it's not overridden

instance {-# OVERLAPPABLE #-} (target ~ (t, base), TreeCataNode a r base target') => TreeCataNode a r target target' where
  cataNode (_, base) = cataNode base

instance {-# OVERLAPPABLE #-} (target ~ (t, base), TreeCataLeaf a r base target') => TreeCataLeaf a r target target' where
  cataLeaf (_, base) = cataLeaf base

instance {-# OVERLAPPABLE #-} (target ~ (t, base), TreeCata a r base target') => TreeCata a r target target'

-- Examples

-- Sum class

instance (TreeCata Integer Integer target target) => TreeCataNode Integer Integer (Sum, ()) target where
  cataNode _ target (Node x y) = cata target x + cata target y

instance (TreeCata Integer Integer target target) => TreeCataLeaf Integer Integer (Sum, ()) target where
  cataLeaf _ _ (Leaf x) = x

instance (TreeCata Integer Integer target target) => TreeCata Integer Integer (Sum, ()) target

-- Plus3 class

instance (TreeCata Integer Integer target target) => TreeCataLeaf Integer Integer (Plus3, (Sum, ())) target where
  cataLeaf _ _ (Leaf x) = x + 3

instance (TreeCata Integer Integer target target) => TreeCata Integer Integer (Plus3, (Sum, ())) target

-- CoolNode class

instance (TreeCata Integer Integer target target) => TreeCataNode Integer Integer (CoolNode, (Plus3, (Sum, ()))) target where
  cataNode _ target (Node x y) = 2 * (cata target x + cata target y)

instance (TreeCata Integer Integer target target) => TreeCata Integer Integer (CoolNode, (Plus3, (Sum, ()))) target

-- Plus5 class

instance (TreeCata Integer Integer target target) => TreeCataLeaf Integer Integer (Plus5, (Plus3, (Sum, ()))) target where
  cataLeaf _ _ (Leaf x) = x + 5

instance (TreeCata Integer Integer target target) => TreeCata Integer Integer (Plus5, (Plus3, (Sum, ()))) target

-- Base class

instance (TreeCata a (Tree a) target target) => TreeCataNode a (Tree a) (Base, ()) target where
  cataNode _ target (Node x y) = Node (cata target x) (cata target y)

instance (TreeCata a (Tree a) target target) => TreeCataLeaf a (Tree a) (Base, ()) target where
  cataLeaf _ _ (Leaf x) = Leaf x

instance (TreeCata a (Tree a) target target) => TreeCata a (Tree a) (Base, ()) target

-- Succ class

instance (Num a, TreeCata a (Tree a) target target) => TreeCataLeaf a (Tree a) (Succ, (Base, ())) target where
  cataLeaf _ _ (Leaf x) = Leaf (x + 1)

instance (Num a, TreeCata a (Tree a) target target) => TreeCata a (Tree a) (Succ, (Base, ())) target

-- Tests

test1 :: Tree Integer
test1 = Node (Node (Leaf 2) (Leaf 7)) (Leaf 5)

test1Sum = cata (Sum, ()) test1
test1Plus3 = cata (Plus3, (Sum, ())) test1
test1Cool = cata (CoolNode, (Plus3, (Sum, ()))) test1
test1Plus5 = cata (Plus5, (Plus3, (Sum, ()))) test1
test1Base = cata (Base, ()) test1
test1Succ = cata (Succ, (Base, ())) test1
