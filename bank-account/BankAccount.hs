module BankAccount where

import           Control.Concurrent.STM

data BankAccount = BankAccount { balance :: TVar Balance }
type Amount      = Int
type Balance     = Maybe Amount


initialBalance :: Balance
initialBalance = Just 0

openAccount :: IO BankAccount
openAccount = atomically $ BankAccount <$> newTVar initialBalance

closeAccount :: BankAccount -> IO ()
closeAccount = atomically . (`writeTVar` Nothing) . balance

getBalance :: BankAccount -> IO Balance
getBalance = atomically . readTVar . balance

incrementBalance :: BankAccount -> Amount -> IO Balance
incrementBalance account = atomically . incrementBalance' (balance account)
  where incrementBalance' :: TVar Balance -> Amount -> STM Balance
        incrementBalance' initial delta = increment initial delta >>= update initial
          where
                increment old  = (<$> readTVar old) . fmap . (+)
                update old new = writeTVar old new >> return new

-- increment :: TVar Balance -> Amount -> STM Balance
-- update :: TVar Balance -> Balance -> STM Balance
