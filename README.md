Module Elements --- elements of free modules.

A  `ModuleElt{K,V}`  represents  an  element  of  a free module where basis
elements  are of type  `K` and coefficients  of type `V`.  Usually you want
objects  of type `V` to be elements of  a ring, but it could also be useful
if  they just belong to  an abelian group. This  is similar to the SageMath
CombinatorialFreeModule.

This  basic  data  structure  is  used  in  my  packages  as  an  efficient
representation   at  many   places.  For   example,  the   `Monomial`  type
representing multivariate monomials is a `ModuleElt{Symbol,Int}`:

`x^2y^-3 ` is represented by `ModuleElt(:x=>2,:y=>-3)`

And  multivariate polynomials are  represented by a `ModuleElt{Monomial,C}`
where `C` is the type of the coefficients:

`x*y-z^2` is represented by ``ModuleElt(x*y=>1,z^2=>-1)

`ModuleElts`  are  also  used  for  cyclotomics, CycPols, elements of Hecke
algebras, etc…

A  `ModuleElt{K,V}` is essentially a  list of `Pairs{K,V}`. The constructor
takes  as argument a list of pairs, or a variable number of pair arguments,
or a generator of pairs.

We provide two implementations:

  - `HModuleElt`, an implementation by `Dict`s 

This  requires  that  the  type  `K`  is  hashable.  It  is  a  very simple
implementation  since the interface of the type  is close to that of dicts;
the  only difference is weeding  out keys which have  a zero cofficient ---
which  is necessary since for testing equality of module elements one needs
a canonical form for each element.

-  a faster implementation  `ModuleElt` is obtained  by keeping the list of
pairs  sorted by key. This demands that the type `K` has a `isless` method.
This  implementation is two  to four times  faster than the  `Dict` one and
requires half the memory.

Both implementations have the same methods, with some exceptions; they have
mostly  the  same  methods  as  a  `Dict`  (`haskey`,  `getindex`,  `keys`,
`values`. `pairs`, `first`, `iterate`, `length`, `eltype`). Adding elements
is a variation on `merge(+,...)` for `Dict`s where keys with zero value are
deleted afterwards (here `+` can be replaced by any operation `op` with the
property  that  `op(0,x)=op(x,0)=x`).  Further,  a  module  element  can be
negated,  or multiplied or divided (`/`or  `//`) by some element (acting on
coefficients)  if the method is defined  between type `V` and that element;
there are also `zero` and `iszero` methods.

The  exception is that  `ModuleElt`s have methods  `cmp` and `isless` which
`HModuleElt`s  don't have. There is also `ModuleElts.merge2` which does the
same as merge but is valid for more general operations.

Here  is an example where basis elements are `Symbol`s and coefficients are
`Int`. As you can see in the examples, at the REPL (or in Jupyter or Pluto,
when  `IO`  has  the  `:limit`  attribute)  the  `show`  method  shows  the
coefficients  (bracketed  if  necessary,  which  is  when  they  have inner
occurences  of `+-*`), followed  by showing the  basis elements. The `repr`
method gives a representation which can be read back in julia:

```julia-repl
julia> a=ModuleElt(:xy=>1,:yx=>-1)
:xy-:yx

julia> repr(a)
"ModuleElt([:xy => 1, :yx => -1])"
```

Setting  the  `IO`  property  `:showbasis`  to  a  custom printing function
changes how the basis elements are printed.

```julia-rep1
julia> show(IOContext(stdout,:showbasis=>(io,s)->string("<",s,">")),a)
3<xy>+2<yx>
```
We illustrate basic operations on `ModuleElt`s:

```julia-repl
julia> a-a
0

julia> a*99
99:xy-99:yx

julia> a//2
(1//2):xy+(-1//2):yx

julia> a/2
0.5:xy-0.5:yx

julia> a+ModuleElt(:yx=>1)
:xy

julia> a[:xy] # indexing by a basis element finds the coefficient
1

julia> a[:xx] # the coefficient of an absent basis element is zero.
0

julia> haskey(a,:xx)
false

julia> first(a)
:xy => 1

julia> collect(a)
2-element Vector{Pair{Symbol, Int64}}:
 :xy => 1
 :yx => -1

julia> collect(keys(a))
2-element Vector{Symbol}:
 :xy
 :yx

julia> collect(values(a))
2-element Vector{Int64}:
  1
 -1

julia> length(a)
2

julia> eltype(a)
Pair{Symbol, Int64}
```

In both implementations the constructor normalizes the constructed element,
removing zero coefficients and merging duplicate basis elements, adding the
corresponding   coefficients  (and   sorting  the   basis  in  the  default
implementation).  If  you  know  this  normalisation is unnecessary, to get
maximum  speed you can disable this  by giving the keyword `check=false` to
the constructor.

```julia-repl
julia> a=ModuleElt(:yy=>1, :yx=>2, :xy=>3, :yy=>-1;check=false)
:yy+2:yx+3:xy-:yy

julia> a=ModuleElt(:yy=>1, :yx=>2, :xy=>3, :yy=>-1)
3:xy+2:yx
```

Adding  or subtracting `ModuleElt`s does promotion  on the type of the keys
and the coefficients if needed:

```julia-repl
julia> a+ModuleElt([:z=>1.0])
3.0:xy+2.0:yx+1.0:z
```
