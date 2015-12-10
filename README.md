# firstclassinstances
Some approaches to implementation of type class inheritance in Haskell.

## Description
``ClassInheritanceBasic.hs`` contains initial approach with less dependencies on GHC's pragmas.

``ClassInheritanceProxified.hs`` uses ``ConstraintKinds`` magic (based on [this article](http://joyoftypes.blogspot.ru/2012/02/haskell-supports-first-class-instances.html)).

Both approaches provide examples on manipulations with the following tree type:

```haskell
data Tree a
  = Node (Tree a) (Tree a)
  | Leaf a
```
