module GameMaster where

import Control.Monad (ap, liftM)

import GameMasterDef

import Data.Maybe (isJust, fromJust)
-- Question 1.

-- The game master for the guessing game.  The parameter is the secret number
-- the player has to guess.
guessingGame :: MonadGameMaster m => Integer -> m Ending
-- Input the secret to start the game
-- Check whether the secret is valid or not (out of range)
-- If so, return error message
-- If not, call the guessing game helper function to start the game
guessingGame secret
    | secret < 1 || secret > 100 = error "invalid game"
    | otherwise = guessing_game_helper 1 100 secret

-- This function specific how the true guessing game work
guessing_game_helper :: MonadGameMaster m => Integer -> Integer -> Integer -> m Ending
guessing_game_helper lowerbound upperbound secret = do
    -- Using the gmAction to get the user/robot input
    -- Test the type of Answer
    answer1 <- gmAction lowerbound upperbound
    -- As demonstrated in handout
    -- If user Surrender, return Lost secret
    -- Otherwise, test the input of user
    -- Four cases:
    -- Case one: user's input is out of range, then call guessing game helper again
    -- Case two: user's input is secret number, then just return Win
    -- Case three: If the user input is less than secret, call guessing game helper and update the lowerbound
    -- Case four: If the user input is more than seceret, call guessing game helper and update the upperbound
    case answer1 of
      Surrender -> return (Lose secret)
      Guess user_inp
        | user_inp < lowerbound || user_inp > upperbound -> guessing_game_helper lowerbound upperbound secret
        | user_inp == secret -> return Win
        | user_inp < secret -> guessing_game_helper (user_inp + 1) upperbound secret
        | user_inp > secret -> guessing_game_helper lowerbound (user_inp - 1) secret





-- Question 2.

instance Functor FreeGameMaster where
    -- fmap :: (a -> b) -> FreeGameMaster a -> FreeGameMaster b
    -- If you are confident with your Monad instance, you can just write
    -- fmap = liftM
    fmap = liftM

instance Applicative FreeGameMaster where
    -- pure :: a -> FreeGameMaster a
    -- If you are confident with your Monad instance, you can just write
    -- pure = return
    pure = return

    -- (<*>) :: FreeGameMaster (a -> b) -> FreeGameMaster a -> FreeGameMaster b
    -- If you are confident with your Monad instance, you can just write
    -- (<*>) = ap
    (<*>) = ap

instance Monad FreeGameMaster where
    -- return :: a -> FreeGameMaster a
    return a = Pure a

    -- (>>=) :: FreeGameMaster a -> (a -> FreeGameMaster b) -> (FreeGameMaster b)
    -- if it is Pure a, then apply function on a.
    -- if it is GMAction, then get the nextState by giving it a user msg and apply it
    -- to the function
    (Pure a) >>= func = func a
    (GMAction lo hi nextState) >>= func = (GMAction lo hi (\msg -> (nextState msg) >>= func))

instance MonadGameMaster FreeGameMaster where
    -- gmAction :: Integer -> Integer -> FreeGameMaster PlayerMsg
    -- the gmAction is just to create FreeGameMaster data type with
    -- the same lower and high bound
    gmAction lo hi = (GMAction lo hi Pure)

              
-- Question 3.
testGame :: (Integer -> FreeGameMaster Ending) -> Bool
testGame gm = isJust (do
    -- Assume you know the secret number is 45
    f1 <- gmActionChecker (gm 45) 1 100
    -- Test whether input 45 can Win
    pureWinChecker (f1 (Guess 45))
    -- Test whether Surrender can return Lose and secrete
    pureLostChecker (f1 Surrender) 45
    -- Test when input is 10 (According to question 1)
    -- Whether the lower bound updated (g + 1)
    f2 <- gmActionChecker (f1 (Guess 10)) 11 100
    -- Test whether input 45 can Win
    pureWinChecker (f2 (Guess 45))
    -- Test whether Surrender can return Lose and secrete
    pureLostChecker (f2 Surrender) 45
    -- Test when input is 99 (According to question 1)
    -- Whether the upper bound updated (g - 1)
    f3 <- gmActionChecker (f2 (Guess 99)) 11 98
    -- Test whether input 45 can Win
    pureWinChecker (f3 (Guess 45))
    -- Test whether Surrender can return Lose and secrete
    pureLostChecker (f3 Surrender) 45
    -- Test whether exceeded range does not effect the range
    f4 <- gmActionChecker (f3 (Guess 101)) 11 98
    -- Test whether input 45 can Win
    pureWinChecker (f4 (Guess 45))
    -- Test whether Surrender can return Lose and secrete
    pureLostChecker (f4 Surrender) 45
    -- Test whether below the range does not effect the range
    f5 <- gmActionChecker (f4 (Guess 10)) 11 98
    -- Test whether input 45 can Win    
    pureWinChecker (f5 (Guess 45))
    -- Test whether Surrender can return Lose and secrete    
    pureLostChecker (f5 Surrender) 45
    -- Test when input is 98 (According to question 1)
    -- Whether the upper bound updated (g - 1)
    f6 <- gmActionChecker (f5 (Guess 98)) 11 97
    pureWinChecker (f6 (Guess 45))
    -- Test when input is 97 (According to question 1)
    -- Whether the upper bound updated (g - 1)
    f7 <- gmActionChecker (f6 (Guess 97)) 11 96
    pureWinChecker (f7 (Guess 45))
    -- Test when input is 12 (According to question 1)
    -- Whether the lower bound updated (g + 1)
    f8 <- gmActionChecker (f7 (Guess 12)) 13 96
    pureWinChecker (f8 (Guess 45))
    -- Combine together (f9, f10). Test whether final
    -- upper bound and lower bound can be the same
    f9 <- gmActionChecker (f8 (Guess 44)) 45 96
    pureWinChecker (f9 (Guess 45))
    f10 <- gmActionChecker (f9 (Guess 46)) 45 45
    pureWinChecker (f10 (Guess 45))
    ) && isJust (do
    -- Assume you know the secret number is 75
    f11 <- gmActionChecker (gm 75) 1 100
    -- Test whether input 75 can Win
    pureWinChecker (f11 (Guess 75))
    -- Test whether Surrender can return Lose and secrete
    pureLostChecker (f11 Surrender) 75
    -- Test when input is 10 (According to question 1)
    -- Whether the lower bound updated (g + 1)
    f12 <- gmActionChecker (f11 (Guess 101)) 1 100
    -- Test whether input 75 can Win
    pureWinChecker (f12 (Guess 75))
    -- Test whether Surrender can return Lose and secrete
    pureLostChecker (f12 Surrender) 75
    -- Test when input is 99 (According to question 1)
    -- Whether the upper bound updated (g - 1)
    f13 <- gmActionChecker (f12 (Guess 99)) 1 98
    -- Test whether input 75 can Win
    pureWinChecker (f13 (Guess 75))
    -- Test whether Surrender can return Lose and secrete
    pureLostChecker (f13 Surrender) 75
    -- Test whether exceeded range does not effect the range
    f14 <- gmActionChecker (f13 (Guess 101)) 1 98
    -- Test whether input 75 can Win
    pureWinChecker (f14 (Guess 75))
    -- Test whether Surrender can return Lose and secrete
    pureLostChecker (f14 Surrender) 75
    -- Test whether below the range does not effect the range
    f15 <- gmActionChecker (f14 (Guess 10)) 11 98
    -- Test whether input 75 can Win    
    pureWinChecker (f15 (Guess 75))
    -- Test whether Surrender can return Lose and secrete    
    pureLostChecker (f15 Surrender) 75
    -- Test when input is 98 (According to question 1)
    -- Whether the upper bound updated (g - 1)
    f16 <- gmActionChecker (f15 (Guess 98)) 11 97
    pureWinChecker (f16 (Guess 75))
    -- Test when input is 97 (According to question 1)
    -- Whether the upper bound updated (g - 1)
    f17 <- gmActionChecker (f16 (Guess 97)) 11 96
    pureWinChecker (f17 (Guess 75))
    -- Test when input is 12 (According to question 1)
    -- Whether the lower bound updated (g + 1)
    f18 <- gmActionChecker (f17 (Guess 12)) 13 96
    pureWinChecker (f18 (Guess 75))
    -- Combine together (f19, f20). Test whether final
    -- upper bound and lower bound can be the same
    f19 <- gmActionChecker (f18 (Guess 74)) 75 96
    pureWinChecker (f19 (Guess 75))
    f20 <- gmActionChecker (f19 (Guess 76)) 75 75
    pureWinChecker (f20 (Guess 75))
    -- Test whether Surrender return Lose and Secret
    pureLostChecker (f20 Surrender) 75
    )


    

-- check whether the user input is Pure Win, if it is, return Just (Win)
-- else return Nothing
pureWinChecker :: FreeGameMaster Ending  -> Maybe Ending
pureWinChecker input = 
    case input of
        Pure Win -> Just (Win)
        GMAction _ _ _ -> Nothing

-- check whether the user input is Pure (Lost n), if it is, then compare 
-- the n with the secret. If it is also equal to the secret, then return Just (Lose). 
-- if it is not the Pure (Lost n), or the n is not
-- equal to the secret, then return Nothing
pureLostChecker :: FreeGameMaster Ending -> Integer -> Maybe Ending
pureLostChecker input secret = 
    case input of
        Pure (Lose n) -> if (n == secret) then Just (Lose n) else Nothing
        GMAction _ _ _ -> Nothing

-- check whether the user input is GMAction. if it is, then check whether the bound is correct
-- if the bound is correct, then return Just (nextState), the nextState is the function
-- to next game state. else return Nothing
gmActionChecker :: FreeGameMaster Ending -> Integer -> Integer -> (Maybe (PlayerMsg -> FreeGameMaster Ending))
gmActionChecker input lo hi =
    case input of
        GMAction lo1 hi1 nextState -> if (lo == lo1 && hi1 == hi) then Just (nextState) else Nothing
        Pure _ -> Nothing




