<a id='ModuleElts' href='#ModuleElts'>#</a>
**`ModuleElts`** &mdash; *Module*.



Module Elements –- elements of free modules.

A  `ModuleElt{K,V}`  represents  an  element  of  a free module where basis elements  are of type  `K` and coefficients  of type `V`.  Usually you want objects of type `V` to be elements of a (non-necessarily commutative) ring, but  it could also be useful if they  just belong to an abelian group. This is  similar to the SageMath CombinatorialFreeModule.  You can also see them as `SparseVector`s where keys can be of type `K` instead of integers.

This  basic  data  structure  is  used  in  my  packages  as  an  efficient representation   at  many   places.  For   example,  the   `Monomial`  type representing multivariate monomials is a `ModuleElt{Symbol,Int}`:

`x^2y^-3` is represented by `ModuleElt(:x=>2,:y=>-3)`

And  multivariate polynomials are  represented by a `ModuleElt{Monomial,C}` where `C` is the type of the coefficients:

`x*y-z^2` is represented by `ModuleElt(x*y=>1,z^2=>-1)`

`ModuleElts`  are  also  used  for  cyclotomics, CycPols, elements of Hecke algebras, etc…

A  `ModuleElt{K,V}` is essentially a  list of `Pairs{K,V}`. The constructor takes  as argument a list of pairs, or a variable number of pair arguments, or a generator of pairs.

We provide two implementations:

  * `HModuleElt`, an implementation by `Dict`s

This  requires  that  the  type  `K`  is  hashable.  It  is  a  very simple implementation  since the interface of the type  is close to that of dicts; the  only difference is weeding  out keys which have  a zero cofficient –- which  is necessary since for testing equality of module elements one needs a canonical form for each element.

  * `ModuleElt`, a faster implementation by a vector of pairs sorted by key.

This  requires that the type `K` has a `isless` method. This implementation is  two to  four times  faster than  the `Dict`  one and  requires half the memory.

Both  implementations  have  the  same  methods,  which are mostly the same methods  as a  `Dict` (`haskey`,  `getindex`, `setindex`, `keys`, `values`. `pairs`,   `first`,  `iterate`,  `length`,  `eltype`,  `copy`),  with  some exceptions.  Adding elements  is implemented  as `merge(+,...)`  which is a variation  on `merge`  for `Dict`s  where keys  with zero value are deleted after  the operation (here `+`  can be replaced by  any operation `op` with the property that `op(0,x)=op(x,0)=x`).

A  module element can also be negated, or multiplied or divided (`/`or `//` or  `\`) by some element (acting on coefficients) if the method is defined between type `V` and that element; the order of the arguments is respected, which  allows to implement  left and right  modules when multiplication for `V` is non-commutative. There are also `zero` and `iszero` methods.

`ModuleElt`s have methods `cmp` and `isless` which `HModuleElt`s don't have (the  definition is lexicographic order). There is also `ModuleElts.merge2` which  does the same as  merge but is valid  for more general operations – thus  is more expensive since it needs  more checks for zero results (I use it with `min` and `max` which implement `gcd` and `lcm` for `Monomial`s and `CycPol`s).

We  show  now  an  an  example;  here  basis  elements  are  `Symbol`s  and coefficients  are `Int`. As you can see in the examples, at the REPL (or in Jupyter  or Pluto, when `IO` has  the `:limit` attribute) the `show` method gives  a nice display where the coefficients (bracketed if necessary, which is  when they have inner occurences of  `+-*/`) preced the keys. The `repr` method gives a representation which can be read back in julia:

```julia-repl
julia> a=ModuleElt(:xy=>1,:yx=>-1)
:xy-:yx

julia> repr(a)
"ModuleElt([:xy => 1, :yx => -1])"

julia> ModuleElt([:xy=>1//2,:yx=>-1])
(1//2):xy+(-1//1):yx
```

Setting  the  `IO`  property  `:showbasis`  to  a  custom printing function changes how the basis elements are printed.

```julia-rep1
julia> show(IOContext(stdout,:limit=>true,:showbasis=>(io,s)->string("<",s,">")),a)
3<xy>+2<yx>
```

We illustrate basic operations on `ModuleElt`s:

```julia-repl
julia> a-a
0

julia> a*99 # Ints commute so same as 99*a
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

julia> haskey(a,:xx) # same as !iszero(a[:xx])
false

julia> a[:xx]=1 # this does an insertion
1

julia> first(a)
:xx => 1

julia> collect(a)
3-element Vector{Pair{Symbol, Int64}}:
 :xx => 1
 :xy => 1
 :yx => -1

julia> collect(keys(a))
3-element Vector{Symbol}:
 :xx
 :xy
 :yx

julia> collect(values(a))
3-element Vector{Int64}:
  1
  1
 -1

julia> length(a)
3

julia> eltype(a)
Pair{Symbol, Int64}
```

In both implementations the constructor normalizes the constructed element, removing zero coefficients and merging duplicate basis elements, adding the corresponding   coefficients  (and   sorting  the   basis  in  the  default implementation).  If  you  know  this  normalisation is unnecessary, to get maximum  speed you can disable this  by giving the keyword `check=false` to the constructor.

```julia-repl
julia> a=ModuleElt(:yy=>1, :yx=>2, :xy=>3, :yy=>-1;check=false)
:yy+2:yx+3:xy-:yy

julia> a=ModuleElt(:yy=>1, :yx=>2, :xy=>3, :yy=>-1)
3:xy+2:yx
```

Adding  or subtracting `ModuleElt`s does promotion  on the type of the keys and the coefficients if needed:

```julia-repl
julia> a+ModuleElt([:z=>1.0])
3.0:xy+2.0:yx+1.0:z
```


<a target='_blank' href='https://github.com/jmichel7/ModuleElts.jl/blob/6864a7ae33c8167bec30dd4f058e1cceab6d7536/src/ModuleElts.jl#L1-L171' class='documenter-source'>source</a><br>

<a id='ModuleElts.ModuleElt' href='#ModuleElts.ModuleElt'>#</a>
**`ModuleElts.ModuleElt`** &mdash; *Type*.



`ModuleElt{K,V}`  has a  similar interface  to `Dict{K,V}`,  but instead of assuming that `K` is hashable, it assumes that `K` is sortable. It also has the advantage that ModuleElts are naturally sortable.

The  only  field,  a  `Vector{Pair{K,V}}`,  is  kept  sorted  by  `K`;  the constructor  by  default  checks  sorting,  adds  values with same key, and suppresses  keys  with  zero  value.  This  can  be bypassed by the keyword `check=false`.


<a target='_blank' href='https://github.com/jmichel7/ModuleElts.jl/blob/6864a7ae33c8167bec30dd4f058e1cceab6d7536/src/ModuleElts.jl#L215-L224' class='documenter-source'>source</a><br>

<a id='Base.merge-Tuple{Function, ModuleElt, ModuleElt}' href='#Base.merge-Tuple{Function, ModuleElt, ModuleElt}'>#</a>
**`Base.merge`** &mdash; *Method*.



`merge(op::Function,a::ModuleElt,b::ModuleElt)`

is  like `merge(op,a,b)` for  `Dict`s, except that  keys with value `0` are deleted after the operation is done.

The  code is only  valid for `op`s  such that `op(0,x)=op(x,0)=x` otherwise the result is wrong. You can use `ModuleElts.merge2` for a (more expensive) function which always works.

`merge(op,m::ModuleElt,b)` does `v->op(v,b)` on the values of `m`.

`merge(op,b,m::ModuleElt)` does `v->op(b,v)` on the values of `m`.

`merge(op,m::ModuleElt)` does `v->op(v)` on the values of `m`.


<a target='_blank' href='https://github.com/jmichel7/ModuleElts.jl/blob/6864a7ae33c8167bec30dd4f058e1cceab6d7536/src/ModuleElts.jl#L253-L268' class='documenter-source'>source</a><br>

<a id='ModuleElts.merge2' href='#ModuleElts.merge2'>#</a>
**`ModuleElts.merge2`** &mdash; *Function*.



`merge2(op::Function,a::ModuleElt,b::ModuleElt)`

does  `op` between coefficients of  the same basis element  in `a` and `b`. This  version works for  general ops (not  necessarily commutative or which need not satisfy op(0,x)=op(x,0)=x), but has too much overhead currently to replace  `merge` for  + or  other ops  such that  op(0,x)==op(x,0)=x. It is useful for max or min which do lcm and gcd of `Monomial`s or `CycPol`s.


<a target='_blank' href='https://github.com/jmichel7/ModuleElts.jl/blob/6864a7ae33c8167bec30dd4f058e1cceab6d7536/src/ModuleElts.jl#L291-L299' class='documenter-source'>source</a><br>

<a id='Base.getindex-Tuple{ModuleElt, Any}' href='#Base.getindex-Tuple{ModuleElt, Any}'>#</a>
**`Base.getindex`** &mdash; *Method*.



`getindex(x::ModuleElt,key)`  returns  the  value  associated  to the given `key` in `x`. It returns zero if the key does not occur in `x`.


<a target='_blank' href='https://github.com/jmichel7/ModuleElts.jl/blob/6864a7ae33c8167bec30dd4f058e1cceab6d7536/src/ModuleElts.jl#L340-L343' class='documenter-source'>source</a><br>

<a id='Base.setindex!-Tuple{ModuleElt, Any, Any}' href='#Base.setindex!-Tuple{ModuleElt, Any, Any}'>#</a>
**`Base.setindex!`** &mdash; *Method*.



`setindex!(x::ModuleElt,v,key)`  sets `v`  as the  value associated  to the given `key` in `x`. Setting a value to zero or for a new key is expensive.


<a target='_blank' href='https://github.com/jmichel7/ModuleElts.jl/blob/6864a7ae33c8167bec30dd4f058e1cceab6d7536/src/ModuleElts.jl#L350-L353' class='documenter-source'>source</a><br>

