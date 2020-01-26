{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Main ( main ) where

import           Control.Exception
import           Data.Aeson
import           Data.Functor.Identity
import           Data.Maybe
import           GHC.Generics
import           HKD.Default

main :: IO ()
main = do
  putStrLn "\n<<<<<<<<<<<<<<<<<<<<<< Test <<<<<<<<<<<<<<<<<<<<<<"
  it "basic" $
    let def = Triple "hello" (Identity 123) pi
        iput = Triple "world" (Just 456) Nothing
        oput = Triple "world" (Identity 456) (Identity pi)
    in oput == applyDef def iput

  it "aeson" $
    let just = decode "{\"name\": {}}"
        nothing = decode "{}" :: Maybe (Person Identity)
        oput = Person { name = Name { first = Identity "Jorah"
                                      , last_ = Identity "Gao"
                                      }
                        , age = Identity 28
                        }
    in just == (Just oput) && nothing == Nothing

  it "type family" $
    let def = Config (Database "127.0.0.1" 3306)
        iput1 = Config Nothing
        iput2 = Config (Just $ Database Nothing Nothing)
        iput3 = Config (Just $ Database (Just "192.168.1.2") Nothing)
        oput = Config (Database "192.168.1.2" 3306)
        applyDefs = map (applyDef def)
    in oput == applyDef def iput3 && all (== def) (applyDefs [iput1, iput2])

  putStrLn "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"


data Triple f = Triple String (f Int) (f Double) deriving Generic

deriving instance Eq (Triple Identity)

instance Default Triple


data Name f = Name { first :: f String
                   , last_ :: f String
                   } deriving Generic

deriving instance Eq (Name Identity)

instance Default Name

instance FromJSON (Name Maybe)


data Person f = Person { name :: Name f -- name is required
                       , age  :: f Int  -- age is optional (can be omitted)
                       } deriving Generic

deriving instance Eq (Person Identity)

instance Default Person

instance FromJSON (Person Maybe)

instance FromJSON (Person Identity) where
  parseJSON v = applyDef def <$> parseJSON v
    where
      def = Person (Name (Identity "Jorah") (Identity "Gao")) (Identity 28)


type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a


data Database f = Database { dbHost :: HKD f String
                           , dbPort :: HKD f Int
                           } deriving Generic

deriving instance Eq (Database Identity)

instance Default Database


data Config f = Config { database :: HKD f (Database f) } deriving Generic

deriving instance Eq (Config Identity)

instance Default Config


it :: String -> Bool -> IO ()
it = flip assert . putStrLn
