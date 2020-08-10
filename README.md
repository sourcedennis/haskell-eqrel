# haskell-eqrel

Several Haskell datastructures that store an equivalence relation for elements. This equivalence relation should be explicitly constructed by equating elements. The functions enforce reflexivity, symmetry, and transitivity on the relation.

Three structures are provided:
* `Data.EqRel` - Stores an equivalence relation for elements implementing `Ord`
* `Data.HashEqRel` - Stores an equivalence relation for elements implementing `Hashable`
* `Data.IntEqRel` - Stores an equivalence relation for elements represented by an `Int`

This implementation is inspired by the paper:

Robert E. Tarjan. (1975). Efficiency of a Good But Not Linear Set Union Algorithm. _JACM 22(2)_

However, it does _not_ have the amortized _O(1)_ time complexity from the paper. Instead, the time complexity is typically amortized _O(log n)_, which is dominated by Set-operations.

***You probably want to use the existing [`union-find`](https://hackage.haskell.org/package/union-find) or [`equivalence`](https://hackage.haskell.org/package/equivalence) packages on Hackage instead***. These implementation _do_ obey the amortized _O(1)_ time complexity by internally using mutable memory.

_Then why did I write this implementation?_ The aforementioned implementations enforce computation takes place within an invasive monad (`IO` / `STT`), which I'd rather not deal with. Those monads make use within multi-threaded applications impossible/cumbersome. Additionally, when non-deterministic computation (with multiple computation paths) occurs, these different path cannot each store their own equivalence classes (as they share memory). For those (quite rare/specific) cases, I'd gladly sacrifice some performance. Hence, this implementation exists.

## Example
The three mentioned structures have almost identical interfaces. This example works with both the `EqRel` and `HashEqRel` modules.

```haskell
main :: IO ()
main =
  do
    print $ fst $ areEquivalent "A" "B" empty -- False
    
    print $ fst $ areEquivalent "A" "A" empty -- True (reflexivity)

    print $ fst $ areEquivalent "A" "B" $ equate "A" "B" empty -- True

    let rel = equate "C" "D" $ equate "A" "B" empty
    print $ fst $ areEquivalent "D" "A" rel -- False

    let rel2 = equate "B" "C" rel
    print $ fst $ areEquivalent "D" "A" rel2 -- True (transitivity)

    let rel3 = equate "X" "Z" $ equate "D" "Y" empty
    print $ fst $ areEquivalent "A" "Y" rel3 -- False

    let rel4 = rel2 `combine` rel3
    print $ fst $ areEquivalent "A" "Y" rel4 -- True (A=D in rel2; D=Y in rel3)
```

## License
BSD-3 - See the `LICENSE` file
