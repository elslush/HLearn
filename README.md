# Homomorphic machine learning

This project is meant to analyze the HLearn framework in Haskell for comparison. Homomorphism allows for extreme parallelization. This concept is explored in this experiment.

## What is Homomorphism?
“A map between two algebraic structures that preserves the operations of the structures”

F (x * y) = F (x) * F (y)

The exponential function x -> ex, 		ex + y = exey

## Monoid Homomorphisms
A homomorphism between two monoids (M, ∗) and (N, •) is a function f : M → N such that

1. f(x ∗ y) = f(x) • f(y) for all x, y in M
2. f(eM) = eN,

where eM and eN are the identities on M and N respectively.

A map between monoids that
1. preserves the monoid operation 
2. maps the identity element of the first monoid to that of the second monoid

## Homomorphisms in machine learning

- parallel batch training -> Monoid
- online training -> Monoid
- fast cross-validation -> Monoid
- "untraining" of data points -> Abelian Group
- more fast cross-validation -> Abelian Group
- weighted data points -> R-Module
- fractionally weighted data points -> Vector Space
- fast simple preprocessing of data -> Functor
- fast complex preprocessing of data -> Monad

References: 
1. https://izbicki.me/public/papers/tfp2013-hlearn-a-machine-learning-library-for-haskell.pdf
2. https://en.wikipedia.org/wiki/Homomorphism
