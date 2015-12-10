{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module ClassInheritanceProxified
 
import Unsafe.Coerce
import Control.Applicative


-- copied from article
-- http://joyoftypes.blogspot.ru/2012/02/haskell-supports-first-class-instances.html
data Dict c where
    Dict :: c => Dict c

withDict :: Dict c -> (c => b) -> b
withDict Dict b = b

newtype Proxy label p = Proxy p

unProxyDict :: Dict (c (Proxy label p)) -> Dict (c p)
unProxyDict = unsafeCoerce

proxyDictFromLabel :: c (Proxy label p) => label -> Dict (c (Proxy label p))
proxyDictFromLabel _ = Dict
 
dictFromLabel :: c (Proxy label p) => label -> Dict (c p)
dictFromLabel a = unProxyDict $ proxyDictFromLabel a 


-- proxy supplimentary
reProxy :: (Proxy l1 p) -> (Proxy l2 p)
reProxy (Proxy p) = Proxy p


-- basic tree 
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show


-- basic tree catamorphism
class TreeNodeHandler res tin | tin -> res where
    handleNode :: tin -> res -> res -> res

class TreeLeafHandler res tin | tin -> res where
    handleLeaf :: tin -> res

class (TreeNodeHandler res (Proxy label tin), TreeLeafHandler res (Proxy label tin)) => 
      CustomizableTreeMap label res tin | label tin -> res where
    mapp :: label -> (Tree tin) -> res
    mapp l (Leaf a) = withDict ((dictFromLabel l) :: Dict (TreeLeafHandler res tin)) $ handleLeaf a
    mapp l (Node a b) = withDict ((dictFromLabel l) :: Dict (TreeNodeHandler res tin)) $ 
                                                         handleNode (undefined :: tin) (mapp l a) (mapp l b)


-- call base method by default 
instance {-# OVERLAPPABLE #-} (BaseLabel l ~ bl, TreeNodeHandler res (Proxy bl tin)) =>
                              TreeNodeHandler res (Proxy l tin) where
    handleNode _ = handleNode (undefined :: (Proxy bl tin))

instance {-# OVERLAPPABLE #-} (BaseLabel l ~ bl, TreeLeafHandler res (Proxy bl tin)) =>
                              TreeLeafHandler res (Proxy l tin) where
    handleLeaf x = handleLeaf ((reProxy x) :: (Proxy bl tin))


-- label's stuff 
type family BaseLabel l 
class LabelWithBase l baseL | l -> baseL

data SummarizeMap = SummarizeMap
data CloneMap = CloneMap

data SwappyMap = SwappyMap 
type instance BaseLabel SwappyMap = CloneMap

data SwappyP1Map = SwappyP1Map
type instance BaseLabel SwappyP1Map = SwappyMap
 
instance TreeNodeHandler Int (Proxy SummarizeMap Int) where
    handleNode _ = (+)
instance TreeLeafHandler Int (Proxy SummarizeMap Int) where
    handleLeaf (Proxy x) = x
instance CustomizableTreeMap SummarizeMap Int Int
 
instance TreeNodeHandler (Tree Int) (Proxy CloneMap Int) where
    handleNode _ = Node
instance TreeLeafHandler (Tree Int) (Proxy CloneMap Int) where
    handleLeaf (Proxy x) = Leaf x
instance CustomizableTreeMap CloneMap (Tree Int) Int

instance TreeNodeHandler (Tree Int) (Proxy SwappyMap Int) where
    handleNode _ a b = Node b a
instance CustomizableTreeMap SwappyMap (Tree Int) Int

instance TreeLeafHandler (Tree Int) (Proxy SwappyP1Map Int) where
    handleLeaf (Proxy x) = Leaf (x + 1)
instance CustomizableTreeMap SwappyP1Map (Tree Int) Int


-- tests
tree :: Tree Int
tree = Node (Node (Leaf 8) (Leaf 0)) (Leaf 2)  

test1 = mapp SummarizeMap tree    -- >   10
test2 = mapp SwappyMap    tree       -- >   Node (Leaf 2) (Node (Leaf 0) (Leaf 8))
test3 = mapp SwappyP1Map  tree     -- >   Node (Leaf 3) (Node (Leaf 1) (Leaf 9))
