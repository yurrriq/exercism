-- |
-- Module      : BankAccount
-- Copyright   : (c) Eric Bailey, 2015
-- License     : MIT
--
-- Maintainer  : Eric Bailey
-- Stability   : experimental
-- Portability : portable
--
-- Accessing thread-safe bank accounts.
module BankAccount
  ( BankAccount,
    openAccount,
    closeAccount,
    getBalance,
    incrementBalance,
  )
where

import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    modifyTVar,
    newTVar,
    readTVar,
    writeTVar,
  )
import Control.DeepSeq (($!!))

-- | A 'BankAccount' holds a shared memory location, representing a 'Balance'
-- that supports atomic memory transactions.
newtype BankAccount
  = -- | Given a 'TVar' 'Balance', return a 'BankAccount'.
    BankAccount
    { -- | Return the memory location of the 'Balance' of a 'BankAccount'.
      balance :: TVar Balance
    }

-- | An 'Amount' is an integer.
type Amount = Integer

-- | A 'Balance' is 'Just' an 'Amount' or 'Nothing'.
type Balance = Maybe Amount

-- | The initial 'Balance' of a 'BankAccount' is 'Just' @0@.
initialBalance :: Balance
initialBalance = Just 0

-- | Atomically "open" (instantiate) a 'BankAccount' with an 'initialBalance'.
openAccount :: IO BankAccount
openAccount = atomically $ BankAccount <$> newTVar initialBalance

-- | Given a 'BankAccount', atomically set its 'Balance' to 'Nothing',
-- effectively prohibiting future transactions, thereby "closing" the account.
closeAccount :: BankAccount -> IO ()
closeAccount = atomically . (`writeTVar` Nothing) . balance

-- | Given a 'BankAccount', call 'readBalance' on it, atomically.
getBalance :: BankAccount -> IO Balance
getBalance = atomically . readBalance

-- | Given a 'BankAccount' and a function @f@ from 'Balance' to 'Balance',
-- mutate the contents of the 'balance' of the bank account by @f@.
modifyBalance :: BankAccount -> (Balance -> Balance) -> STM ()
modifyBalance = modifyTVar . balance

-- | Given a 'BankAccount', call 'balance' on it, and read the current value.
readBalance :: BankAccount -> STM Balance
readBalance = readTVar . balance

-- | Given a 'BankAccount' and an 'Amount' (potentially negative), atomically
-- add the amount to the account's 'balance' and return the updated 'Balance'.
incrementBalance :: BankAccount -> Amount -> IO Balance
incrementBalance account amount =
  atomically $
    (modifyBalance account $!! fmap (+ amount))
      >> readBalance account
