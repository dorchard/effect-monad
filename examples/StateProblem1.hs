{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

import Control.Monad.State.Lazy

-- inc :: (MonadState s m, Num s) => m ()

inc :: (Monad m, Num s) => StateT s m ()
inc = do x <- get
         put (x + 1)

-- append :: (MonadState [a] m) => [a] -> m ()

append :: (Monad m) => [a] -> StateT [a] m ()
append y = do x <- get
              put (x ++ y)



-- need to add lifts, must make some arbitrary choice

prog1 = do inc
           lift $ append "world"

runProg1 = runStateT (runStateT prog1 41) "hello " -- need to get the order right 
                                                 -- but types will help us 
         
neg = do x <- get
         put (not x)

foo2 = do lift $ neg
          inc
          lift $ lift $ append "world"
          
          
runFoo2 = runStateT (runStateT (runStateT foo2 41) False) "hello "

prog2 = do inc
           lift $ append "world"
           lift $ lift $ append "Nice to"

runProg2 = runStateT (runStateT (runStateT prog2 41) "Hello ") " meet you"

foo3 = do lift $ neg
          inc
          lift $ lift $ append "world"
          lift $ lift $ lift $ append "hoogle "

-- same type makes it possibly confusing
runFoo3 = runStateT (runStateT (runStateT (runStateT foo3 41) False) "hello ") " woogle"