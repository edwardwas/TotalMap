Total Map
===============

Often one may have an enum type representing all possible keys of something, and wishes to store some data associated with it. In this case you have two options - use a function or a Map from containers. A function works, but can be difficult to update and has not Eq or Show instances. A map solves this issue, but gives no guarantee that a key has associated data - all functions become partial. This library offers a different way of solving this problem.

A `TotalMap k a` is a total mapping from a key of type `k` to a value of type `a`; each `k` will have exactly one `a`. It permits many instances, including `Show` and `Eq`. 

Example
------

Let us create and example. We start with some imports and some language pragmas.

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import TotalMap

import Control.Lens
import Control.Monad (void)
import Data.Functor.Rep (tabulate)
import qualified GHC.Generics as GHC (Generic)
import Generics.SOP
```

TotalMap uses generics-sop internally, so we require it as an import. We also require some typeclasses introduced by lens.

For our example, we shall create a dummy program for sending out peoples fortunes based on their star sign. We shall create a star sign type.

```haskell
data StarSign
  = Aries
  | Taurus
  | Gemini
  | Cancer
  | Leo
  | Virgo
  | Scorpio
  | Sagittarius
  | Capricorn
  | Aquarius
  | Pisces
  deriving (Eq, Show, GHC.Generic, Generic)
```

Note the derivation of both `GHC.Generic` and `Generic`. These are required to guarantee that `StarSign`'s constructors take no imports.

We have a list of people, and we can partition them based on their star sign.

```haskell
data Date = Date
  { month :: Int
  , day :: Int
  } deriving (Eq, Show)

data Person = Person
  { name :: String
  , email :: String
  , birthDate :: Date
  } deriving (Eq, Show)

signFromDate :: Date -> StarSign
signFromDate = undefined

peopleSign :: [Person] -> TotalMap StarSign [Person]
peopleSign ps = tabulate $ \s -> filter ((==) s . signFromDate . birthDate) ps
```

We could send people an email with their fortune.

```haskell
sendFortune :: String -> String -> StarSign -> IO ()
sendFortune = undefined

sendFortunes :: [Person] -> IO ()
sendFortunes =
  void .
  itraverse (\sign -> mapM_ (\p -> sendFortune (name p) (email p) sign)) .
  peopleSign
```

Future
-----

* Come up with a better example
* Is Lens necessary. It is useful, but it is a huge dependency.


The following is required to make tests compile.

```haskell
main :: IO ()
main = return ()
```
