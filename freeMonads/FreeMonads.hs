module FreeMonads () where

import Control.Monad.Free

data StackF a = Push Int a
              | Pop (Int -> a)

instance Functor StackF where
  fmap f (Push n a) = Push n (f a)
  fmap f (Pop g) = Pop (f . g)

type Stack = Free StackF

push :: Int -> Stack ()
push n = liftF $ Push n ()

pop :: Stack Int
pop = liftF $ Pop id

runStack :: Stack a -> [Int] -> (a, [Int])
runStack (Pure a) s = (a, s)
runStack (Free (Push n a)) s = runStack a (n:s)
runStack (Free (Pop f)) (s:ss) = runStack (f s) ss
runStack (Free (Pop _)) [] = error "Empty stack"


--- Example of Stack, should return (20, [10]) when run with runStack and the empty list
example :: Stack Int
example = do
  push 10
  push 20
  pop

--- Example of Stack, should return an error when run with runStack
errorExample :: Stack Int
errorExample = do
  push 10
  pop
  pop
