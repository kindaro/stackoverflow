> {-# LANGUAGE DeriveFunctor, ViewPatterns,  PatternSynonyms, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, FlexibleContexts, UndecidableInstances #-}

codereview.stackexchange.com/q/57985

> module AstTyping where
>
> import Control.Monad.State
> import Control.Applicative

The basic parameterised type

> data ExpF a =
>     AddF a a
>   | NumF Int deriving (Show, Functor)
>
> data Exp  = Exp (ExpF Exp) deriving (Show)
> data ExpH = Holey (Either Hole (ExpF ExpH)) deriving (Show)
> data ExpLine = Line Int (ExpF (ExpLine)) deriving (Show)
>
> data Hole = Hole deriving (Show)

View Typeclass. There isn't an instance which makes sense for ExpH

> class ASTView a where
>   view :: a -> ExpF a
>
> instance ASTView Exp where
>   view (Exp x) = x
>
> instance ASTView ExpLine where
>   view (Line _ x) = x

Generic pattern matches - note that we maintain the interface!

> pattern Add a b <- (view -> (AddF a b))
> pattern Num n <- (view -> (NumF n))

Building type class

> class ASTBuild a  where
>   construct :: ExpF a -> a 
>
> instance ASTBuild Exp where
>   construct x = Exp x
>
> instance MonadState Int m => ASTBuild (m ExpLine)  where
>   construct (AddF a b) = do
>     n <- get 
>     a' <- a
>     b' <- b
>     return $ (Line n (AddF a' b'))
>   construct (NumF v) = do
>     n <- get
>     inc 
>     return $ (Line n (NumF v))
>
> inc :: MonadState Int m => m ()
> inc = modify (+1)
>
> instance ASTBuild ExpH where
>   construct = Holey . Right

Generic Constructors, direct drop in for functions in the builder

> add :: ASTBuild f => f -> f  -> f
> add a b = construct (AddF a b)
>
> num :: ASTBuild f => Int -> f
> num n = construct (NumF n)

Examples 

1. Normal

> example1 :: Exp
> example1 = (num 5) `add` (num 6)

2. Holey - Allows you to leave holes in the structure to fill in later.
This could be a direct replacement to the fmonad.

> holeExample :: ExpH
> holeExample = add (num 6) hole
>
> example2 :: Exp
> example2 = removeHoles holeExample
>
> hole :: ExpH
> hole = Holey (Left Hole)
>
> removeHoles :: ExpH -> Exp
> removeHoles (Holey (Left Hole)) = num 5
> removeHoles (Holey (Right k)) = recurse removeHoles k

> -- This is a fold
> recurse :: (a -> Exp) -> ExpF a -> Exp
> recurse f (AddF e1 e2) = add (f e1) (f e2)
> recurse _ (NumF n) = num n

3. Adding additional information is also easy, this example labels each leaf with a unique number. Could be used for attrs on every node or keeping track of line numbers.

> leafCounter :: State Int ExpLine
> leafCounter = add (num 5) (num 6)
>
> example3 :: ExpLine
> example3 = evalState leafCounter 0

All 3 examples are then evaluated with the same function

> eval :: (ASTView v) => v -> Int
> eval (Add a b) = eval a + eval b
> eval (Num n) = n
>
> main :: IO ()
> main = 
>   do
>     print (eval example1) 
>     print (eval example2) 
>     print (eval example3) 
