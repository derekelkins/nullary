{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
module Class.Unsafe (
    Unsafe,
    unsafeUnsafe
) where
import Class.Nullary

-- Not exported.
data UnsafeTag

-- | Nullary type class to mark unsafe functions, like unsafePerformIO.
class Unsafe

instance Tag UnsafeTag => Unsafe

-- | Unsafely ignore Unsafe marker.
unsafeUnsafe :: (Unsafe => a) -> a
unsafeUnsafe = unsafeTag (Proxy :: Proxy UnsafeTag)
{-# INLINE unsafeUnsafe #-}
