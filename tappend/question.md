Type level monoid?
==================

Insofar as ℕ is an _(additive)_ monoid, vectors length indexed with ℕ form a monoid too. Is there
a way to generalize it to any monoid-indexed type?

There is not a standard definition of length indexed vectors in Haskell so far, but let us
consider the one from `vector-sized` as a real world example. I say:

    apvend :: Vector m a -> Vector n a -> Vector (m + n) a
    apvend = (Data.Vector.Sized.++)
    
    empvy :: Monoid a => Vector 0 a
    empvy = mempty

I have a thought that similar operations may be defined for other ℕ-indexed types too. ℕ-ary
functions to a monoid come to mind, since a polyary function to `a` may be represented with a type
family like this:

    type family F (ts :: [*]) a
      where
        F '[ ] a = a
        F (t ': ts) a = t -> F ts a

Since type level lists are a _(free)_ monoid, we can imagine a function like this:

    apfend f g = undefined
    apfend :: F ts m -> F ts' m -> F (ts <> ts') m

I was not able to make it compile, since it appears to require a type family dependency like this:

    type family F (ts :: [*]) a = r | r a -> ts

— Which is not yet available.
