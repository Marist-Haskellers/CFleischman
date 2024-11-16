-- import Control.Monad.State

-- type State s a = (s -> (a, s))

-- newtype State s a = State (s -> (a, s))

-- data State s a = State (s -> (a,s ))

-- s0 = 5

-- f n = (even n, 2*n)
-- :t f
-- ghci> f :: Integral b => -> b (Bool, b)

-- st = State family

-- runState st s0
-- ghci> (False,10)

--