"""
Module Elements --- elements of free modules.

A  `ModuleElt{K,V}` represents an element of  a free module where the basis
elements  are of type `K` and the coefficients are of type `V`. Usually you
want  objects of type `V` to be elements of a (not necessarily commutative)
ring,  but it can also  be useful if they  just belong to an abelian group.
This is similar to the SageMath CombinatorialFreeModule. You can also think
of  them as `SparseVector`s, where  the keys can be  of type `K` instead of
integers.

This  basic data  structure is  used in  many places  in my  packages as an
efficient   representation.  For   example,  the   type  `Monomial`,  which
represents multivariate monomials is a `ModuleElt{Symbol,Int}`:

`x^2y^-3 ` is represented by `ModuleElt(:x=>2,:y=>-3)`

And  multivariate polynomials are  represented by a `ModuleElt{Monomial,C}`
where `C` is the type of the coefficients:

`x*y-z^2` is represented by `ModuleElt(x*y=>1,z^2=>-1)`

`ModuleElts`  are  also  used  for  cyclotomics, CycPols, elements of Hecke
algebras, etc…

A  `ModuleElt{K,V}` is essentially a  list of `Pairs{K,V}`. The constructor
takes  as argument a list of pairs, or a variable number of pair arguments,
or a generator of pairs.

We provide two implementations:

  - `HModuleElt`, an implementation by `Dict`s 

This  requires  the  type  `K`  to  be  hashable.  This  is  a  very simple
implementation  since the interface of the type  is close to that of dicts;
the  only difference  is that  keys with  cofficient zero are discarded ---
which  is necessary, since for checking the equality of module elements one
needs a canonical form for each element.

  - `ModuleElt`, a faster implementation by a vector of pairs sorted by key.

This requires that the type `K` has an `isless` method. This implementation
is  two to  four times  faster than  the `Dict` implementation and requires
half the memory.

Both  implementations  have  the  same  methods,  which are mostly the same
methods  as a  `Dict` (`haskey`,  `getindex`, `setindex`, `keys`, `values`.
`pairs`,   `first`,  `iterate`,  `length`,  `eltype`,  `copy`),  with  some
exceptions.  Adding elements  is implemented  as `merge(+,...)`  which is a
variation  on  `merge`  for  `Dict`s  where  keys with coefficient zero are
discarded  after the operation  (here `+` can  be replaced by any operation
`op` with the property that `op(0,x)=op(x,0)=x`).

A  module element can also be negated, or multiplied or divided (`/`or `//`
or  `\\`) by any element (acting on  coefficients) if the method is defined
between  the  type  `V`  and  that  element;  the order of the arguments is
respected,  which  allows  to  implement  left  and  right  modules  if the
multiplication  is  not  commutative  for  `V`.  There  are also `zero` and
`iszero` methods.

`ModuleElt`s have methods `cmp` and `isless` which `HModuleElt`s don't have
(the  definition is lexicographic order). There is also `ModuleElts.merge2`
which  does the same as  merge but is valid  for more general operations --
thus  is more expensive since it needs  more checks for zero results (I use
it with `min` and `max` which implement `gcd` and `lcm` for `Monomial`s and
`CycPol`s).

We  now show an an  example; here the basis  elements are `Symbol`s and the
coefficients  are `Int`. As you can see  from the examples, at the REPL (or
in  Jupyter  or  Pluto,  when  `IO`  has the `:limit` attribute) the `show`
method gives a nice display where the coefficients (bracketed if necessary,
that  is when they have inner occurrences  of `+-*/`) precede the keys. The
`repr` method gives a representation which can be read back in julia:

```julia-repl
julia> a=ModuleElt(:xy=>1,:yx=>-1)
:xy-:yx

julia> repr(a)
"ModuleElt([:xy => 1, :yx => -1])"

julia> ModuleElt([:xy=>1//2,:yx=>-1])
(1//2):xy+(-1//1):yx
```

Setting  the  `IO`  property  `:showbasis`  to  a  custom printing function
changes how the basis elements are printed.

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

In both implementations the constructor normalises the constructed element,
removing zero coefficients and merging duplicate basis elements, adding the
corresponding   coefficients  (and   sorting  the   basis  in  the  default
implementation).  If you know  that this normalisation  is unnecessary, you
can  disable it for  maximum speed by  passing the keyword `check=false` to
the constructor.

```julia-repl
julia> a=ModuleElt(:yy=>1, :yx=>2, :xy=>3, :yy=>-1;check=false)
:yy+2:yx+3:xy-:yy

julia> a=ModuleElt(:yy=>1, :yx=>2, :xy=>3, :yy=>-1)
3:xy+2:yx
```

Adding  or subtracting `ModuleElt`s does promotion  on the type of the keys
and the coefficients, if needed:

```julia-repl
julia> a+ModuleElt([:z=>1.0])
3.0:xy+2.0:yx+1.0:z
```
"""
module ModuleElts

export ModuleElt, HModuleElt # data structure

#------------- implementation with Dicts ----------------------
struct HModuleElt{K,V} <: AbstractDict{K,V}
  d::Dict{K,V} # the keys K should be hashable
  function HModuleElt(d::Dict{K,V};check::Bool=true)where {K,V}
    if check
      for (k,v) in d if iszero(v) delete!(d,k) end end
    end
    new{K,V}(d)
  end
end

function HModuleElt(x::Vector{Pair{K,V}};check=true) where{K,V}
  if !check || length(x)<=1 return HModuleElt(Dict(x)) end
  res=Dict{K,V}()
  for (k,v) in x
    if haskey(res,k) res[k]+=v 
    else res[k]=v 
    end
  end
  HModuleElt(res)
end

# forwarded methods
Base.haskey(x::HModuleElt,y...)=haskey(x.d,y...)
Base.keys(x::HModuleElt)=keys(x.d)
Base.values(x::HModuleElt)=values(x.d)

Base.getindex(x::HModuleElt{K,V},i) where{K,V}=haskey(x,i) ?  x.d[i] : zero(V)
function Base.setindex!(x::HModuleElt{K,V},v,i) where{K,V}
  if iszero(v) delete!(x.d,v) else x.d[i]=v end
  x
end

# Valid for ops such that op(0,x)=op(x,0)=x otherwise the result is wrong.
Base.merge(op::Function,a::HModuleElt,b::HModuleElt)=HModuleElt(merge(op,a.d,b.d))

# since merge2 not implemented
Base.:-(a::HModuleElt,b::HModuleElt)=a+(-b) 
#-------------- faster implementation -------------------------------------
"""
`ModuleElt{K,V}`  has a  similar interface  to `Dict{K,V}`,  but instead of
assuming  that `K` is hashable, it assumes  that `K` is sortable. This also
has the advantage that ModuleElts are naturally sortable.

The  only field, a `Vector{Pair{K,V}}`, is  kept sorted by `K`; by default,
the  constructor checks sorting, adds values with the same key, and deletes
keys with zero value. This can be overriden with the keyword `check=false`.
"""
struct ModuleElt{K,V} <: AbstractDict{K,V}
  d::Vector{Pair{K,V}}
  function ModuleElt(d::AbstractVector{Pair{K,V}};check::Bool=true)where {K,V}
    if check normalize!(d) end
    new{K,V}(d)
  end
end

function normalize!(d::Vector{<:Pair})
  if isempty(d) return end
  if !issorted(d,by=first) sort!(d,by=first) end
  i=1
  ki,vi=d[i]
@inbounds for j in 2:length(d)
    if first(d[j])==ki
      vi+=last(d[j])
      d[i]=ki=>vi
    else 
      if !iszero(vi) i+=1 end
      ki,vi=d[i]=d[j]
    end
  end
  resize!(d,iszero(vi) ? i-1 : i)
end

Base.cmp(x::ModuleElt,y::ModuleElt)=cmp(x.d,y.d)
Base.isless(x::ModuleElt,y::ModuleElt)=cmp(x,y)==-1

"""
`merge(op::Function,a::ModuleElt,b::ModuleElt)`

is  like `merge(op,a,b)` for  `Dict`s, except that  keys with value `0` are
deleted after the operation is done.

The  code is only  valid for `op`s  such that `op(0,x)=op(x,0)=x` otherwise
the result is wrong. You can use `ModuleElts.merge2` for a (more expensive)
function which always works.
"""
function Base.merge(op::Function,a::ModuleElt,b::ModuleElt)
  (a,b)=promote(a,b)
  la=length(a.d)
  lb=length(b.d)
  res=similar(a.d,la+lb)
  ai=bi=1
  ri=0
@inbounds while ai<=la || bi<=lb
    if     ai>la res[ri+=1]=b.d[bi]; bi+=1
    elseif bi>lb res[ri+=1]=a.d[ai]; ai+=1
    else c=cmp(first(a.d[ai]),first(b.d[bi]))
      if     c>0 res[ri+=1]=b.d[bi]; bi+=1
      elseif c<0 res[ri+=1]=a.d[ai]; ai+=1
      else s=op(last(a.d[ai]),last(b.d[bi]))
        if !iszero(s) res[ri+=1]=first(a.d[ai])=>s end
        ai+=1; bi+=1
      end
    end
  end
  ModuleElt(resize!(res,ri);check=false)
end

"""
`ModuleElts.merge2(op::Function,a::ModuleElt,b::ModuleElt)`

does  `op` between coefficients of  the same basis element  in `a` and `b`.
This  version works  for general  ops (not  necessarily commutative  or not
satisfying  op(0,x)=op(x,0)=x).  It  currently  has  too  much  overhead to
replace  `merge` for  + or  other ops  such that  op(0,x)==op(x,0)=x. It is
useful for max or min which do lcm and gcd of `Monomial`s or `CycPol`s.
"""
function merge2(op::Function,a::ModuleElt,b::ModuleElt)
  (a,b)=promote(a,b)
  la=length(a.d)
  lb=length(b.d)
  res=similar(a.d,la+lb)
  ai=bi=1
  ri=0
@inbounds while ai<=la || bi<=lb
    if     ai>la 
      be=b.d[bi]
      s=op(zero(last(be)),last(be))
      if !iszero(s) res[ri+=1]=first(be)=>s end
      bi+=1
    elseif bi>lb 
      ae=a.d[ai]
      s=op(last(ae),zero(last(ae)))
      if !iszero(s) res[ri+=1]=first(ae)=>s end
      ai+=1
    else c=cmp(first(a.d[ai]),first(b.d[bi]))
      if     c>0 
        be=b.d[bi]
        s=op(zero(last(be)),last(be))
        if !iszero(s) res[ri+=1]=first(be)=>s end
        bi+=1
      elseif c<0 
        ae=a.d[ai]
        s=op(last(ae),zero(last(ae)))
        if !iszero(s) res[ri+=1]=first(ae)=>s end
        ai+=1
      else s=op(last(a.d[ai]),last(b.d[bi]))
        if !iszero(s) res[ri+=1]=first(a.d[ai])=>s end
        ai+=1; bi+=1
      end
    end
  end
  ModuleElt(resize!(res,ri);check=false)
end

Base.:-(a::ModuleElt,b::ModuleElt)=merge2(-,a,b) # faster than a+(-b)

"""
`getindex(x::ModuleElt,key)`  returns the value in  `x` associated with the
`given key`. It returns zero if the key does not occur in `x`.
"""
function Base.getindex(x::ModuleElt{K,V},i) where {K,V}
  r=searchsortedfirst(x.d,i=>zero(V);by=first)
  if r>length(x.d) || first(x.d[r])!=i return zero(V) end
  last(x.d[r])
end

"""
`setindex!(x::ModuleElt,v,key)`  sets `v`  as the  value in  `x` associated
with  the  given  `key`.  Setting  a  value  to  zero  or  for a new key is
expensive.
"""
function Base.setindex!(x::ModuleElt{K,V},v,key) where {K,V}
  r=searchsortedfirst(x.d,key=>zero(V);by=first)
  if r>length(x.d) || first(x.d[r])!=key 
    if !iszero(v) splice!(x.d,r:r-1,[key=>v]) end
  elseif iszero(v) deleteat!(x.d,r)
  else x.d[r]=key=>v
  end
  x
end

function Base.haskey(x::ModuleElt{K,V},i) where {K,V}
  r=searchsortedfirst(x.d,i=>zero(V);by=first)
  r<=length(x.d) && first(x.d[r])==i
end

Base.pairs(x::ModuleElt)=x.d
Base.keys(x::ModuleElt)=(first(p) for p in x.d)
Base.values(x::ModuleElt)=(last(p) for p in x.d)

# The next function takes as argument a string representing a coefficient.
# Tries to determine if it should be bracketed for unambiguous display
function format_coefficient(c::String)
  if c=="1" ""
  elseif c=="-1" "-"
  elseif match(r"^[-+]?([^-+*/]|√-|{-)*(\(.*\))?$",c)!==nothing c
  else "("*c*")" 
  end
end

#-------------- methods which have same code in both implementations-------

for M in (:HModuleElt, :ModuleElt)
  @eval begin

# we assume that converting the keys from K to K1 does not change hashing
# for the Dict implementation or sorting for the pairs implementation
function Base.convert(::Type{$M{K,V}},a::$M{K1,V1}) where {K,K1,V,V1}
  if K==K1
    if V==V1 a
    elseif iszero(a) zero($M{K,V})
    else $M(k=>convert(V,v) for (k,v) in a.d;check=false)
    end
  else 
    if iszero(a) zero($M{K,V})
    elseif V==V1
      $M(convert(K,k)=>v for (k,v) in a.d;check=false)
    else
      $M(convert(K,k)=>convert(V,v) for (k,v) in a.d;check=false)
    end
  end
end

$M(x::Pair...;u...)=$M(collect(x);u...)
$M(x::Base.Generator;u...)=$M(collect(x);u...)

Base.:+(a::$M,b::$M)=merge(+,a,b)
Base.:-(a::$M)=iszero(a) ? a : $M(k=>-v for(k,v) in a;check=false)

Base.push!(x::$M,p::Pair)=setindex!(x,p[2]+getindex(x,p[1]),p[1])

# multiply module element on left or right by scalar
function Base.:*(a::$M{K,V},b)where {K,V}
  if iszero(b) || iszero(a) 
    return zero($M{K,promote_type(V,typeof(b))})
  end
  let b=b
    $M(k=>v*b for (k,v) in a;check=false)
  end
end

function Base.:*(b,a::$M{K,V})where {K,V}
  if iszero(b) || iszero(a) 
    return zero($M{K,promote_type(V,typeof(b))})
  end
  let b=b
    $M(k=>b*v for (k,v) in a;check=false)
  end
end

Base.iszero(x::$M)=isempty(x.d)
Base.zero(x::$M)=$M(empty(x.d))
Base.zero(::Type{$M{K,V}}) where{K,V}=$M(Pair{K,V}[])
# forwarded methods
Base.:(==)(a::$M,b::$M)=a.d==b.d
@inline Base.iterate(x::$M,y...)=iterate(x.d,y...) # inline make a difference
Base.length(x::$M)=length(x.d)
Base.hash(x::$M, h::UInt)=hash(x.d,h)
Base.copy(x::$M)=$M(copy(x.d);check=false)

function Base.promote_rule(a::Type{$M{K1,V1}},
                           b::Type{$M{K2,V2}})where {K1,K2,V1,V2}
  $M{promote_type(K1,K2),promote_type(V1,V2)}
end

Base.show(io::IO, ::MIME"text/plain", m::$M)=show(io,m)

function Base.show(io::IO,m::$M)
  if !(get(io,:limit,false) || get(io,:TeX,false))
    print(io,Symbol($M),"(",m.d,")")
    return
  end
  if iszero(m) print(io,"0"); return end
  showbasis=get(io,:showbasis,nothing)
  if isnothing(showbasis) 
    showbasis=(io,x)->repr(x;context=io)
  end
  start=true
  res=""
  for (k,v) in m 
    v=repr(v;context=io)
    k=showbasis(io,k)
    if !isempty(k) v=format_coefficient(v) end
    if (isempty(v) || v[1]!='-') && !start v="+"*v end
    res*=v*k
    if start start=false end
  end
  if res=="" res="1" end
  print(io,res)
end

# if iszero(m) needs that zero(V) is defined
function oncoeffs(f::Function,m::$M{K,V},b)where {K,V}
  if iszero(m) return zero($M{K,typeof(f(zero(V),b))}) end
  p=(k=>f(v,b) for (k,v) in m)
  $M(p;check=any(x->iszero(last(x)),p))
end

# if iszero(m) needs that zero(V) is defined
function oncoeffs(f::Function,b,m::$M{K,V})where {K,V}
  if iszero(m) return zero($M{K,typeof(f(b,zero(V)))}) end
  p=(k=>f(b,v) for (k,v) in m)
  $M(p;check=any(x->iszero(last(x)),p))
end

function oncoeffs(f::Function,m::$M)
  p=(k=>f(v) for (k,v) in m)
  $M(p;check=any(x->iszero(last(x)),p))
end

Base.conj(m::$M)=oncoeffs(conj,m)
Base.:/(m::$M,b)=oncoeffs(/,m,b)
Base.:(//)(m::$M,b)=oncoeffs(//,m,b)
Base.:\(b,m::$M)=oncoeffs(\,b,m)
Base.div(m::$M,b)=oncoeffs(div,m,b)
using LinearAlgebra: LinearAlgebra, exactdiv
LinearAlgebra.exactdiv(m::$M,b)=oncoeffs(exactdiv,m,b)
Base.min(m::$M,n::$M)=merge2(min,m,n)
Base.max(m::$M,n::$M)=merge2(max,m,n)

# next 8 methods to resolve ambiguities
Base.:*(a::$M,b::$M)=error("ambiguous")
oncoeffs(f::Function,a::$M,b::$M)=error("ambiguous")
end
end
Base.merge(f::Function,a::ModuleElt,b::HModuleElt)=error("ambiguous")
Base.merge(f::Function,a::HModuleElt,b::ModuleElt)=error("ambiguous")
"""
`ModuleElts.oncoeffs(op,m::ModuleElt,b)` does `v->op(v,b)` on the values of `m`.

`ModuleElts.oncoeffs(op,b,m::ModuleElt)` does `v->op(b,v)` on the values of `m`.

`ModuleElts.oncoeffs(op,m::ModuleElt)` does `v->op(v)` on the values of `m`.
"""
oncoeffs(f::Function,a::ModuleElt,b::HModuleElt)=error("ambiguous")
oncoeffs(f::Function,a::HModuleElt,b::ModuleElt)=error("ambiguous")

Base.:*(a::ModuleElt,b::HModuleElt)=error("ambiguous")
Base.:*(a::HModuleElt,b::ModuleElt)=error("ambiguous")
end
