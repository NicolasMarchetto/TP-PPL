# MiniPPL Haskell

## Description

MiniPPL is a probabilistic programming language implemented in Haskell. The project allows for the definition of probabilistic models and the execution of various inference algorithms.

The language supports random variables, observations, probability distributions, and mathematical primitives. Execution is handled by a custom abstract machine that interprets the programs and enables the application of different inference methods.

The implemented algorithms are:

- Likelihood Weighting (LW).
- Sequential Monte Carlo (SMC).
- Single Site Metropolis-Hastings (SSMH).

---

# Requirements

- GHC
- Cabal

---

# Execution
From the project folder:

In terminal write cabal run

Then choose any test case:

1 - Formula
2 - SSMH / Likelihood Weighting / SMC
3 - One Coin Enumeration
4 - Bits8 Enumeration

If you choose 2 then:
Select inference algorithm:
1 - SSMH
2 - Likelihood Weighting (LW)
3 - Sequential Monte Carlo (SMC)

---

