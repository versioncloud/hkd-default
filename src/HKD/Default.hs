{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}

{-|When I use "Data.Aeson" library to decode a json string into a Haskell value,
I want to provide default values for @Maybe@ fields, which will be Nothing
when these fields are omitted in the json string. It's a hard work when there
are lots of @Maybe@ fields or deeply nested fields, it will also make your code
hard to read and to maintain. This module provides a solution by using
Higher-kinded data (HKD).
See [this blog](https://reasonablypolymorphic.com/blog/higher-kinded-data/)
for more information about @HKD@.

For example, if you have a @Config@ type as follows,

@
data Config = Config { dbHost :: String
                     , dbPort :: Int
                     , dbName :: String
                       ...
                     }

@

and you want to read these configuration data from a json file when you start
you application, you instantiate "Data.Aeson.FromJSON" for the @Config@,

@
data Config = Config { dbHost :: String
                     , dbPort :: Int
                     , dbName :: String
                       ...
                     } deriving Generic

instance FromJSON Config
@

and you want 'dbPort' can be omitted in the json string, a default value will be
used when it is omitted, you change @String@ type to @Maybe String@,

@
data Config = Config { dbHost :: String
                     , dbPort :: Maybe Int
                     , dbName :: String
                       ...
                     } deriving Generic

instance FromJSON Config
@

and decode and use the configuration data in main function as below,

@
main = do
  config \<- fromJust \<$\> decodeFileStrict "./config.json"
  let host = dbHost config
      port = fromMaybe defaultDBPort $ dbPort config
      ...
  dbConn <- connectDB host port ...
  ...
@

it is neither elegant nor easy to maintain when you have lots of configuration
items.

By using @HKD@ and @type family@, it becomes easier to maintain your code.

@
type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a

data ConfigH f = Config { dbHost = String
                        , dbPort = HKD f String
                          ...
                        } deriving Generic

instance Default ConfigH

instance FromJSON (ConfigH Maybe)

type Config = Config Identity

instance FromJSON Config where
  parseJSON v = applyDef def <$> parseJSON v
    where
      def = Config undefined 3306 ...


main = do
  -- Enable RecordWildCards extension
  Config{..} \<- fromJust \<$\> decodeFileStrict "./config.json"
  dbConn <- connectDB dbHost dbPort ...
@

=== More Examples

>>> :set -XDeriveGeneric
>>> :set -XFlexibleInstances
>>> :set -XStandaloneDeriving
>>> import           Data.Functor.Identity
>>> import           GHC.Generics
>>> import           HKD.Default
>>> :{
data Triple f = Triple String (f Int) (f Double) deriving Generic
instance Default Triple
deriving instance Show (Triple Identity)
:}
...
>>> let def = Triple "hello" (Identity 123) pi :: Triple Identity
>>> applyDef def $ Triple "world" (Just 456) Nothing
Triple "world" (Identity 456) (Identity 3.141592653589793)

>>> :set -XDeriveGeneric
>>> :set -XStandaloneDeriving
>>> :set -XFlexibleInstances
>>> :set -XOverloadedStrings
>>> import           Data.Aeson
>>> import           Data.Functor.Identity
>>> import           GHC.Generics
>>> import           HKD.Default
>>> :{
data Name f = Name { first :: f String
                   , last_ :: f String
                   } deriving Generic
instance Default Name
deriving instance Show (Name Identity)
instance FromJSON (Name Maybe)
data Person f = Person { name :: Name f -- name is required
                       , age  :: f Int  -- age is optional (can be omitted)
                       } deriving Generic
instance Default Person
deriving instance Show (Person Identity)
instance FromJSON (Person Maybe)
instance FromJSON (Person Identity) where
  parseJSON v = applyDef def <$> parseJSON v
    where
      def = Person (Name (Identity "Jorah") (Identity "Gao")) (Identity 28)
:}
>>> decode "{\"name\": {}}" :: Maybe (Person Identity)
Just (Person {name = Name {first = Identity "Jorah", last_ = Identity "Gao"}, age = Identity 28})
>>> decode "{}" :: Maybe (Person Identity)
Nothing
-}
module HKD.Default
  ( Default(..)
  ) where


import           Data.Functor.Identity
import           Data.Maybe
import           GHC.Generics


-- | In most cases, use the default implementation for 'Generic' instance.
class Default (t :: (* -> *) -> *) where
  applyDef :: t Identity -> t Maybe -> t Identity

  default applyDef :: ( Generic (t Identity)
                      , Generic (t Maybe)
                      , GDefault (Rep (t Identity)) (Rep (t Maybe))
                      )
                   => t Identity -> t Maybe -> t Identity
  applyDef i m = to $ gapplyDef (from i) (from m)


class GDefault f g where
  gapplyDef :: f (t Identity) -> g (t Maybe) -> f (t Identity)

-- Data type
instance GDefault f g => GDefault (D1 c f) (D1 c g) where
  gapplyDef (M1 p) (M1 k) = M1 $ gapplyDef p k

-- Choice between data constructors
instance ( GDefault f g
         , GDefault f' g'
         ) => GDefault (f :+: f') (g :+: g') where
  gapplyDef (L1 p) (L1 k) = L1 $ gapplyDef p k
  gapplyDef (R1 p) (R1 k) = R1 $ gapplyDef p k

-- Data constructor
instance ( Constructor c
         , GDefault f g
         ) => GDefault (C1 c f) (C1 c g) where
  gapplyDef (M1 p) (M1 k) = M1 $ gapplyDef p k

-- Enum type (nullary data constructor)
instance Constructor c => GDefault (C1 c U1) (C1 c U1) where
  gapplyDef (M1 p) (M1 k) = M1 p

-- Apply record selectors
instance ( GDefault f g
         , GDefault f' g'
         ) => GDefault (f :*: f') (g :*: g') where
  gapplyDef (p :*: p') (k :*: k') = (gapplyDef p k) :*: (gapplyDef p' k')

-- Selector
instance (Selector c , GDefault f g) => GDefault (S1 c f) (S1 c g) where
  gapplyDef (M1 p) (M1 k) = M1 $ gapplyDef p k

-- Not nested required field
instance GDefault (K1 i f) (K1 i f) where
  gapplyDef (K1 p) (K1 k) = K1 k

-- Not nested optional field (use type family)
instance GDefault (K1 i f) (K1 i (Maybe f)) where
  gapplyDef (K1 p) (K1 k) = K1 $ fromMaybe p k

-- Not nested optional field (not use type family)
instance GDefault (K1 i (Identity f)) (K1 i (Maybe f)) where
  gapplyDef (K1 p) (K1 Nothing)  = K1 p
  gapplyDef (K1 p) (K1 (Just k)) = K1 $ Identity k

-- Nested required field
instance Default t => GDefault (K1 i (t Identity)) (K1 i (t Maybe)) where
  gapplyDef (K1 p) (K1 k) = K1 $ applyDef p k

-- Nested optional field
instance Default t => GDefault (K1 i (t Identity)) (K1 i (Maybe (t Maybe))) where
  gapplyDef (K1 p) (K1 Nothing)  = K1 p
  gapplyDef (K1 p) (K1 (Just k)) = K1 $ applyDef p k
