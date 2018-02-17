module SecretHandshake (handshake) where

import           Data.Bits (testBit)


data Event = Wink
           | DoubleBlink
           | CloseYourEyes
           | Jump
           | Reverse
           deriving (Enum, Bounded)


instance Show Event where
  show Wink          = "wink"
  show DoubleBlink   = "double blink"
  show CloseYourEyes = "close your eyes"
  show Jump          = "jump"
  show Reverse       = "reverse"


fromInt :: Int -> [Event]
fromInt n = [ event | event <- Reverse : [Wink .. Jump]
                    , n `testBit` fromEnum event ]


handshake :: Int -> [String]
handshake = foldr go [] . fromInt
  where
    go :: Event -> [String] -> [String]
    go Reverse = reverse
    go event   = (show event :)
