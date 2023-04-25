{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Person
  ( Address (..),
    Born (..),
    Name (..),
    Person (..),
    bornStreet,
    renameStreets,
    setBirthMonth,
    setCurrentStreet,
  )
where

import Control.Lens
  ( makeLenses,
    over,
    set,
    view,
  )
import Data.Time.Calendar (Day, fromGregorian, toGregorian)

data Person = Person
  { _name :: Name,
    _born :: Born,
    _address :: Address
  }

data Name = Name
  { _foreNames :: String,
    _surName :: String
  }

data Born = Born
  { _bornAt :: Address,
    _bornOn :: Day
  }

data Address = Address
  { _street :: String,
    _houseNumber :: Int,
    _place :: String,
    _country :: String
  }

makeLenses ''Person
makeLenses ''Born
makeLenses ''Address

bornStreet :: Born -> String
bornStreet = view (bornAt . street)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet = set (address . street)

setBirthMonth :: Int -> Person -> Person
setBirthMonth monthOfYear = over (born . bornOn) $ \day ->
  let (year, _, dayOfMonth) = toGregorian day
   in fromGregorian year monthOfYear dayOfMonth

renameStreets :: (String -> String) -> Person -> Person
renameStreets rename =
  over (born . bornAt . street) rename
    . over (address . street) rename
