{-|
Module      : BankAccount
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Accessing thread-safe bank accounts.
-}

module BankAccount where

import           Control.Concurrent.STM (STM, TVar, atomically, newTVar,
                                         readTVar, writeTVar)

-- | A 'BankAccount' holds a shared memory location, representing a 'Balance'
-- that supports atomic memory transactions.
data BankAccount =
  -- | Given a 'TVar' 'Balance', return a 'BankAccount'.
  BankAccount {
    -- | Return the memory location of the 'Balance' of a 'BankAccount'.
    balance :: TVar Balance
    }

-- | An 'Amount' is an integer.
type Amount  = Int

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

-- | Given a 'BankAccount', call 'balance' on it, and atomically read the
-- current value.
getBalance :: BankAccount -> IO Balance
getBalance = atomically . readTVar . balance

-- | Given a 'BankAccount' and an 'Amount' (potentially negative) to add to its
-- 'Balance', atomically add the amount to the balance and return the updated
-- 'Balance'.
incrementBalance :: BankAccount -> Amount -> IO Balance
incrementBalance account = atomically . incrementBalance' (balance account)
  where incrementBalance' :: TVar Balance -> Amount -> STM Balance
        incrementBalance' initial delta = increment initial delta >>= update initial
          where
            increment :: TVar Balance -> Amount -> STM Balance
            increment old  = (<$> readTVar old) . fmap . (+)
            update :: TVar Balance -> Balance -> STM Balance
            update old new = writeTVar old new >> return new
