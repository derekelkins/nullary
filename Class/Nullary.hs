{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
----------------------------------------------------------------------------
-- |
-- Module     : Class.Nullary
-- Copyright  : 2015 Derek Elkins
-- License    : BSD2
--
-- Maintainer  : Derek Elkins <derek.a.elkins@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Provides a framework for defining and using nullary type classes
-- without needing orphan instances.  To do this requires some evil
-- to locally generate a type class instances.  This library 
-- encapsulates that evil and provides a mechanism for safely 
-- defining new nullary type classes.
--
-- To define a nullary type class, you use the following pattern:
--
-- > -- The following four extensions are necessary.
-- > {-# LANGUAGE FlexibleContexts #-}
-- > {-# LANGUAGE MultiParamTypeClasses #-}
-- > {-# LANGUAGE Rank2Types #-}
-- > {-# LANGUAGE UndecidableInstances #-}
-- >
-- > -- Not exported unless you want to allow users to opt out of
-- > -- the checking by making an instance for Tag PartialTag.
-- > data PartialTag
-- > 
-- > -- The nullary type class. It can have members, but it's not
-- > -- clear this accomplishes anything.
-- > class Partial
-- > 
-- > -- Enable this library.  Instances like this unfortunately
-- > -- require UndecidableInstances.
-- > instance Tag PartialTag => Partial
-- > 
-- > -- Wrap unsafeTag for user convenience.
-- > partial :: (Partial => a) -> a
-- > partial = unsafeTag (Proxy :: Proxy PartialTag)
-- > {-# INLINE partial #-}
-- > 
-- > -- Define your functions using the Partial class.
-- > head :: Partial => [a] -> a
-- > head (x:xs) = x
-------------------------------------------------------------------------------
module Class.Nullary ( 
    Tag, 
    unsafeTag,
    Proxy(..) -- re-exported for convenience
) where
import Data.Proxy ( Proxy(..) )
import Unsafe.Coerce ( unsafeCoerce )

-- | Class for declaring tagged nullary instances.  See module description.
class Tag t

-- The evil.

newtype MagicTag t r = MagicTag (Tag t => r)

-- | Unsafely cast off the Tag constraint.  This should only
-- be used at the "top-level" of an application.  In practice, 
-- specializations of this should be provided, e.g. unsafeUnsafe.
-- This uses the same evil as Data.Reflect.
unsafeTag :: forall r t. Proxy t -> (Tag t => r) -> r
unsafeTag Proxy f = unsafeCoerce (MagicTag f :: MagicTag t r) ()
{-# INLINE unsafeTag #-}
