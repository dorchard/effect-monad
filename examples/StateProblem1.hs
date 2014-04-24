{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

import Control.Monad.State.Lazy

-- inc :: (MonadState s m, Num s) => m ()

inc = do x <- get
         put (x + 1)

-- append :: (MonadState [a] m) => [a] -> m ()

append y = do x <- get
              put (x ++ y)



-- need to add lifts, must make some arbitrary choice

foo = do inc
         lift $ append "world"

runFoo = runStateT (runStateT foo 41) "hello " -- need to get the order right 
                                               -- but types will help us 
         
neg = do x <- get
         put (not x)

foo2 = do lift $ neg
          inc
          lift $ lift $ append "world"
          
          
runFoo2 = runStateT (runStateT (runStateT foo2 41) False) "hello "


foo3 = do lift $ neg
          inc
          lift $ lift $ append "world"
          lift $ lift $ lift $ append "hoogle "

-- same type makes it possibly confusing
runFoo3 = runStateT (runStateT (runStateT (runStateT foo3 41) False) "hello ") " woogle"