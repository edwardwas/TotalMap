{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module      : TotalMap
Description : A total mapp
Copyright   : (c) Ed Wastell, 2018
License     : MTL
Maintainer  : edward@wastell.com
Stability   : experimental

A total map from an enum type. Consult the README for more information
-}

module TotalMap
    ( TotalMap()
    , generateAllConstructors
    , allTags
    , getTotalMap
    , setTotalMap
    , ixTotal
    , IsEnumType
    ) where

import           Control.Applicative  (liftA2)
import           Control.Lens         (FoldableWithIndex (..),
                                       FunctorWithIndex (..), Lens',
                                       TraversableWithIndex (..), itoList, lens)
import           Data.Distributive    (Distributive (..))
import           Data.Functor.Classes (Eq1 (..), Show1 (..))
import           Data.Functor.Rep     (Representable (..))
import           Data.List            (intercalate)
import           Generics.SOP

-- | Generate all constructors for some enum type.
--
-- TODO: This uses undefined internally as I can not convince the type checker that every constructor has no arguments. This shouldn't be an issue, but feels unsafe so probably should be changed
generateAllConstructors :: IsEnumType tag => NP (K tag) (Code tag)
generateAllConstructors = hliftA2 aux (hcpure (Proxy @SListI) $ hpure undefined) injections
  where
    aux np (Fn inj) = K (to (SOP $ unK $ inj np))

-- | A `TotalMap` is a total mapping from some enum type `tag` to some value `a`: it is isomorphic to `tag -> a`. It uses a generics-sop `NP` array to store all values, ensuring that every value of `tag` must have a corresponding value.
data TotalMap (tag :: *) (a :: *) where
    TotalMap :: (IsEnumType tag) => NP (K a) (Code tag) -> TotalMap tag a

-- | A `TotalMap` where each value is its own key. This is the equivalent of `id`.
allTags :: IsEnumType tag => TotalMap tag tag
allTags = TotalMap generateAllConstructors

--totalMapWithTag :: IsEnumType tag => TotalMap tag a -> TotalMap tag (tag, a)
--totalMapWithTag = liftA2 (,) allTags

instance Functor (TotalMap tag) where
    fmap f (TotalMap np) = TotalMap $ hliftA (mapKK f) np

instance IsEnumType tag => FunctorWithIndex tag (TotalMap tag) where
    imap f tm = f <$> allTags <*> tm

instance (IsEnumType tag) => Applicative (TotalMap tag) where
    pure a = TotalMap $ hpure (K a)
    liftA2 f (TotalMap a) (TotalMap b) = TotalMap $ hliftA2 (mapKKK f) a b

instance IsEnumType tag => Monad (TotalMap tag) where
  tm >>= f = imap (\tag a -> getTotalMap (f a) tag) tm

instance Foldable (TotalMap tag) where
    foldMap f (TotalMap np) = foldMap f $ hcollapse np

instance IsEnumType tag => FoldableWithIndex tag (TotalMap tag) where
    ifoldMap f tm = foldMap (uncurry f) ((,) <$> allTags <*> tm)

instance Traversable (TotalMap tag) where
    sequenceA (TotalMap np) = TotalMap <$> hsequenceK np

instance IsEnumType tag => TraversableWithIndex tag (TotalMap tag) where
    itraverse func tm = traverse (uncurry func) ( (,) <$> allTags <*> tm)

instance IsEnumType tag => Distributive (TotalMap tag) where
  distribute = imap (\tag -> fmap (\tm -> getTotalMap tm tag)) . pure

instance IsEnumType tag => Representable (TotalMap tag) where
  type Rep (TotalMap tag) = tag
  index = getTotalMap
  tabulate func = TotalMap $ hmap (mapKK func) generateAllConstructors

instance (IsEnumType tag) => Eq1 (TotalMap tag) where
    liftEq f a b = foldr (&&) True (f <$> a <*> b)

instance (IsEnumType tag, Eq a) => Eq (TotalMap tag a) where
    (==) = liftEq (==)

instance (IsEnumType tag, Show tag) => Show1 (TotalMap tag) where
    liftShowsPrec f _ n tm ss =
        "TotalMap [" ++
        intercalate
            ", "
            (map (\(t, a) -> "(" ++ show t ++ "," ++ f n a "" ++ ")") $
             itoList tm) ++
        "]" ++ ss

instance (IsEnumType tag, Show a, Show tag) => Show (TotalMap tag a) where
    showsPrec n = liftShowsPrec showsPrec undefined n

-- | Extract a value out of a `TotalMap`
getTotalMap :: TotalMap tag a -> tag -> a
getTotalMap (TotalMap tm) a = hcollapse (hapInjs tm !! (hindex $ from a))

-- | Replace a value inside a `TotalMap`
setTotalMap ::
       forall tag a. IsEnumType tag
    => TotalMap tag a
    -> tag
    -> a
    -> TotalMap tag a
setTotalMap (TotalMap tm) tag a =
    let helper :: NP (K a) xss -> NS (NP f) xss -> NP (K a) xss
        helper (k :* as) (S z) = k :* helper as z
        helper (_ :* as) (Z _) = (K a) :* as
        helper Nil _           = error "Unreachable"
     in TotalMap (helper tm (unSOP $ from tag))

-- | A `Lens` into a value of a `TotalMap`
ixTotal :: IsEnumType tag => tag -> Lens' (TotalMap tag a) a
ixTotal tag = lens (\tm -> getTotalMap tm tag) (\tm -> setTotalMap tm tag)
