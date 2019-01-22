{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE TypeApplications  #-}

module Port
  where

import           GHC.Generics
import           Clash.Prelude
import           Data.Proxy
import           Data.List.NonEmpty                       ( NonEmpty(..) )

class HasPortName a where
  getPortName :: Proxy a -> String -> PortName
  default getPortName :: (Generic a, HasPortName' (Rep a)) => Proxy a -> String -> PortName
  getPortName _ prefix = case getPortName' (Proxy @(Rep a)) prefix of
                           x:|[] -> x
                           x:|xs -> PortProduct "" (x:xs)

class HasPortName' (a :: * -> *) where
  getPortName' :: Proxy a -> String -> NonEmpty PortName

instance HasPortName' a => HasPortName' (D1 m a) where
  getPortName' (_ :: Proxy (D1 m a)) = getPortName' (Proxy @a)

instance HasPortName' a => HasPortName' (C1 m a) where
  getPortName' (_ :: Proxy (C1 m a)) prefix =
    case getPortName' (Proxy @a) prefix of
      x:|[] -> x :| []
      x:|xs -> PortProduct "" (x:xs) :| []

instance (HasPortName' a, HasPortName' b) => HasPortName' (a :*: b) where
  getPortName' (_ :: Proxy (a :*: b)) prefix =
    getPortName' (Proxy @a) prefix <> getPortName' (Proxy @b) prefix

instance KnownSymbol n => HasPortName' (S1 ('MetaSel ('Just n) x y z) (Rec0 (Signal d a))) where
  getPortName' (_ :: Proxy (S1 ( 'MetaSel ( 'Just n) x y z) (Rec0 (Signal d a)))) prefix
    = (PortName $ prefix <> symbolVal (Proxy @n)) :| []
