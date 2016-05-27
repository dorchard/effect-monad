{-# LANGUAGE FlexibleContexts, DataKinds, TypeOperators,
              RebindableSyntax, FlexibleInstances #-}

import Prelude hiding (Monad(..))
import Control.Effect
import Control.Effect.State

c1 = Var::(Var "c1")
c2 = Var::(Var "c2")
out  = Var::(Var "out")

incC :: Var v -> State '[v :-> Int :! RW] ()
incC var = do x <- get var
              put var (x + 1)

writeS :: [a] -> State '["out" :-> [a] :! RW] ()
writeS y = do x <- get out
              put out (x ++ y)

write ::  [a] -> State '["c1" :-> Int :! RW, "out" :-> [a] :! RW] ()
write x = do  writeS x
              incC c1

initState0 = Ext (c1 :-> ((0::Int) :! Eff)) (Ext (out :-> ("" :! Eff)) Empty)
runWrite = runState (write "hello") initState0

hellow :: State '["c1" :-> Int :! RW, "nom" :-> String :! R, "out" :-> String :! RW] ()
hellow = do write "hello"
            write " "
            name <- get (Var::(Var "nom"))
            write (name::String)


-- appendBuffer :: String -> State '["buff" :-> String :! RW] ()
appendBuffer x = do let bvar = Var::(Var "buff")
                    buff <- get bvar
                    put bvar (buff ++ x)

hello :: State '["buff" :-> String :! RW, "name" :-> String :! R] ()
hello = do name <- get (Var::(Var "name"))
           appendBuffer $ "hello " ++ name

initState = Ext (c1 :-> ((0::Int) :! Eff)) (Ext (out :-> ("" :! Eff)) Empty)
runHellow = runState hellow initState

hellowCount = do hellow
                 incC c2

initState' = Ext (c1 :-> ((0::Int) :! Eff)) (Ext (c2 :-> ((1::Int) :! Eff)) (Ext (out :-> ("" :! Eff)) Empty))
runHellowCount = runState hellowCount initState'

-- Show instances

instance Show (Var "out") where
    show _ = "out"

instance Show (Var "c1") where
    show _ = "c1"

instance Show (Var "c2") where
    show _ = "c2"
