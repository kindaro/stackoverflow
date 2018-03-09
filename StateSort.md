You're in luck: `State` is the easiest monad to understand.

Please do not get discouraged by the following fact: your function does not need `State` at all, insofar as you use `minimum` from the standard library.

    myFunct' :: Ord a => [a] -> ([a], a)
    myFunct' xs = (reverse xs, minimum xs)

(It would run like this:)

    λ myFunct' [1,2,3]
    ([3,2,1],1)

Notice though that, in order for you to apply both `reverse` and `minimum` to a list, you will need to traverse it two times. This is when `State` may get handy: using it, you can only traverse the list once, thus, hopefully, gaining speedup. Read on to find out how.

So, **`State` is a function** of a special kind: the thing you give it (also called "state") is kept in a magic box where you can observe it, or replace it with another thing of the same type at any time. You may also think of `State` as an imperative procedure and of "state" as a local variable. Let us review the tools that you may use to construct and execute a `State`:

* You may **observe** the thing in the box with the *(inappropriately named)* function `get`. Notice that this does not change the state in any way − what you obtain is merely an immutable copy of its current value; the thing stays in the box.
  
  You would usually associate your observation with a name, then use it as an ordinary value − for example, pass it to a pure function:

        stateExample1 :: State Integer Integer
        stateExample1 = do
            x <- get  -- This is where we observe state and associate it with the name "x".
            return $ x * 2  -- (* 2) is an example of a pure function.

  &nbsp;

        λ runState stateExample1 10
        (20,10)  -- The first is the return value, the second is the (unchanged) state.

* You may **replace** the thing in the box with another suitably typed thing; use the function `put`:

        stateExample2 :: State Integer Integer
        stateExample2 = do
            x <- get
            put $ x * 2  -- You may think of it as though it were "x = x * 2" 
                         -- in an imperative language.
            return x

  &nbsp;

        λ runState stateExample2 10
        (10,20)  -- Now we have changed the state, and return its initial value for reference.

  Notice that, though we changed the state, our observation of it (that we named "x") still has the same value.

* You may **run** the `State` function, giving it an argument (we'd call it "_initial state_"):

        y = runState stateExample1 10

  − Is the same as
        
        y = stateExample1(10);

  − in an imperative language with C-like syntax, except that you obtain both the return value and the *final state*.

Armed with this knowledge, we can now straighten your proposed `myFunct` like this:

    myFunct :: Ord a => [a] -> State (Maybe a) [a]
    myFunct [ ] = return [ ]
    myFunct t = do
            let s = reverse t
            let a = minimum t
            put (Just a)
            return s

&nbsp;

    λ runState (myFunct [1,2,3]) (Just (-100))
    ([3,2,1],Just 1)
    λ runState (myFunct []) (Just (-100))
    ([],Just (-100))

If we regard `State` as an imperative procedure, then the reversed list is what it returns, while the minimum of the list is what its final state would be. As the list may be empty, we have provisioned an optional default value for the minimum. This makes the function *total*, which is considered good Haskell style:

    λ myFunct' []
    ([],*** Exception: Prelude.minimum: empty list
    λ runState (myFunct []) Nothing
    ([],Nothing)


