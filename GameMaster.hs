module GameMaster where

import Control.Monad (ap, liftM)

import GameMasterDef

-- Question 1.

-- The game master for the guessing game.  The parameter is the secret number
-- the player has to guess.
guessingGame :: MonadGameMaster m => Integer -> m Ending
guessingGame secret
    | secret < 1 || secret > 100 = error "invalid game"
    | otherwise = nextTurn 1 100
    where 
        nextTurn low high = do
            req1 <- gmAction low high
            case req1 of
                Surrender -> return (Lose secret)
                Guess i
                    | i == secret -> return Win
                    | i > high || i < low -> nextTurn low high
                    | i > secret -> nextTurn low (i-1)
                    | otherwise -> nextTurn (i+1) high


-- Question 2.

instance Functor FreeGameMaster where
    -- fmap :: (a -> b) -> FreeGameMaster a -> FreeGameMaster b
    -- If you are confident with your Monad instance, you can just write
    -- fmap = liftM
    -- fmap f (FreeGameMaster x) = FreeGameMaster (f x)

instance Applicative FreeGameMaster where
    -- pure :: a -> FreeGameMaster a
    -- If you are confident with your Monad instance, you can just write
    -- pure = return
    -- pure x = FreeGameMaster x

    -- (<*>) :: FreeGameMaster (a -> b) -> FreeGameMaster a -> FreeGameMaster b
    -- If you are confident with your Monad instance, you can just write
    -- (<*>) = ap

instance Monad FreeGameMaster where
    -- return :: a -> FreeGameMaster a

    -- (>>=) :: FreeGameMaster a -> (a -> FreeGameMaster b) -> (FreeGameMaster b)

instance MonadGameMaster FreeGameMaster where
    -- gmAction :: Integer -> Integer -> FreeGameMaster PlayerMsg


-- Question 3.

testGame :: (Integer -> FreeGameMaster Ending) -> Bool
testGame = error "TODO"
