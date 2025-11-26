# ModuleElts
Elements of free modules.

A  `ModuleElt{K,V}` represents an element of  a free module where the basis
elements  are of type `K` and the coefficients are of type `V`. Usually you
want  objects of type `V` to be elements of a (not necessarily commutative)
ring,  but it can also  be useful if they  just belong to an abelian group.
This is similar to the SageMath CombinatorialFreeModule. You can also think
of  them as `SparseVector`s, where  the keys can be  of type `K` instead of
integers.

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://jmichel7.github.io/ModuleElts.jl/stable/)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://jmichel7.github.io/ModuleElts.jl/dev/)
[![Build Status](https://github.com/jmichel7/ModuleElts.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/jmichel7/ModuleElts.jl/actions/workflows/CI.yml?query=branch%3Amain)
