{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module Generics.Instant.Functions.Hashable
  ( -- $defaults
    ghashWithSalt
    -- * Internals
  , GHashable
  ) where

import           Data.Hashable (Hashable(hashWithSalt))
import           Generics.Instant

--------------------------------------------------------------------------------
-- $defaults
--
-- You can use 'ghashWithSalt' as your generic 'hashWithSalt' for any
-- 'Representable' type as follows:
--
-- @
-- instance 'Hashable' MyType where hashWithSalt = 'ghashWithSalt'
-- @

ghashWithSalt :: (Representable a, GHashable (Rep a)) => Int -> a -> Int
ghashWithSalt = \s a -> ghashWithSalt' s (from a)
{-# INLINABLE ghashWithSalt #-}

--------------------------------------------------------------------------------

class GHashable a where
  ghashWithSalt' :: Int -> a -> Int

instance GHashable Z where
  ghashWithSalt' _ _ = error
    "Generics.Instant.Functions.Hashable.GHashable Z ghashWithSalt' - impossible"

instance GHashable U where
  ghashWithSalt' s U = hashWithSalt s ()
  {-# INLINABLE ghashWithSalt' #-}

instance GHashable a => GHashable (CEq c p q a) where
  ghashWithSalt' s (C a) = ghashWithSalt' s a
  {-# INLINABLE ghashWithSalt' #-}

instance (GHashable a, GHashable b) => GHashable (a :*: b) where
  ghashWithSalt' s (a :*: b) = ghashWithSalt' (ghashWithSalt' s a) b
  {-# INLINABLE ghashWithSalt' #-}

instance (GHashable a, GHashable b) => GHashable (a :+: b) where
  ghashWithSalt' s lr = 0 `hashWithSalt` case lr of
    L a -> Left  (ghashWithSalt' s a)
    R b -> Right (ghashWithSalt' s b)
  {-# INLINABLE ghashWithSalt' #-}

instance Hashable a => GHashable (Var a) where
  ghashWithSalt' s (Var a) = hashWithSalt s a
  {-# INLINABLE ghashWithSalt' #-}

instance Hashable a => GHashable (Rec a) where
  ghashWithSalt' s (Rec a) = hashWithSalt s a
  {-# INLINABLE ghashWithSalt' #-}
