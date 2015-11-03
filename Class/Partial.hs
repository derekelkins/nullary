{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
module Class.Partial (
    Partial,
    PartialTag,
    partial
) where
import Class.Nullary

-- | Prefer 'partial'.  However, if you want to disable Partial
-- wholesale, make an orphan instance of Tag for this type.
-- NEVER put an instance of this in a library.
data PartialTag

-- | Nullary type class to mark partial functions, e.g. head.
class Partial

instance Tag PartialTag => Partial

-- | Accept the possibility of partiality.
partial :: (Partial => a) -> a
partial = unsafeTag (Proxy :: Proxy PartialTag)
{-# INLINE partial #-}
