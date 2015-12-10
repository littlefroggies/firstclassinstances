# firstclassinstances
Some approaches on implementation of type class inheritance in Haskell.

## Description
``ClassInheritanceBasic.hs`` contains initial approach with lesser dependencies on GHC's pragmas.

Second example will be added soon.

Both approaches provide examples on manipulations with the following tree type:

```haskell
data Tree a
  = Node (Tree a) (Tree a)
  | Leaf a
```
