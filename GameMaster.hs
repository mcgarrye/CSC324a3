module GameMaster where

import Control.Monad (ap, liftM)
import Data.Maybe

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
    fmap = liftM

instance Applicative FreeGameMaster where
    -- pure :: a -> FreeGameMaster a
    -- If you are confident with your Monad instance, you can just write
    pure = return

    -- (<*>) :: FreeGameMaster (a -> b) -> FreeGameMaster a -> FreeGameMaster b
    -- If you are confident with your Monad instance, you can just write
    (<*>) = ap

instance Monad FreeGameMaster where
    -- return :: a -> FreeGameMaster a
    return a = Pure a

    -- (>>=) :: FreeGameMaster a -> (a -> FreeGameMaster b) -> (FreeGameMaster b)
    Pure a >>= k = k a
    GMAction low high next >>= k =  GMAction low high (\message -> (next message >>= k))

instance MonadGameMaster FreeGameMaster where
    -- gmAction :: Integer -> Integer -> FreeGameMaster PlayerMsg
    gmAction low high = GMAction low high (\message -> Pure message)


-- Question 3.

testGame :: (Integer -> FreeGameMaster Ending) -> Bool
testGame game = isJust(testcase game)

testcase :: (Integer -> FreeGameMaster Ending) -> Maybe()
testcase game = do 
    f1 <- thirdHelper 1 100 (game 20)
    firstHelper (f1 (Guess 20))
    secondHelper (f1 Surrender) 20
    f2 <- thirdHelper 11 100 (f1 (Guess 10))
    firstHelper (f2 (Guess 20))
    secondHelper (f2 Surrender) 20
    f3 <- thirdHelper 11 67 (f2 (Guess 68))
    firstHelper (f3 (Guess 20))
    secondHelper (f3 Surrender) 20
    f4 <- thirdHelper 11 67 (f3 (Guess 70))
    firstHelper (f4 (Guess 20))
    secondHelper (f4 Surrender) 20
    f5 <- thirdHelper 11 67 (f3 (Guess 200))
    firstHelper (f5 (Guess 20))
    secondHelper (f5 Surrender) 20



firstHelper :: FreeGameMaster Ending -> Maybe()
firstHelper (Pure Win) = Just()
firstHelper _ = Nothing

secondHelper :: FreeGameMaster Ending -> Integer -> Maybe()
secondHelper (Pure (Lose n)) secret
    | n == secret = Just()
    | otherwise = Nothing
secondHelper _ secret = Nothing

thirdHelper :: Integer -> Integer -> FreeGameMaster Ending -> Maybe(PlayerMsg -> FreeGameMaster Ending)
thirdHelper low high (GMAction i j k)
    | (low == i) && (high == j) = Just k
    | otherwise = Nothing
thirdHelper _ _ (Pure _) = Nothing
