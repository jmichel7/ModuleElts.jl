# Property tests for ModuleElts.jl.
#
# The package's own test file replays the docstring examples, which pins printing
# and a few behaviours but says nothing about the invariants the type relies on.
# These tests check, for both implementations:
#
#   * the canonical form (distinct keys, no zero coefficients, and sorted keys
#     for ModuleElt) survives every operation;
#   * the *contents* of every operation agree with a Dict-based reference;
#   * the AbstractDict protocol (getindex/haskey/get/setindex!/push!/keys) works;
#   * min/max, convert and promotion behave;
#   * equal elements hash equally (no ordering assumption: HModuleElt keys need
#     not be sortable).
#
# Nothing outside Test and the package is needed, so this file can be included
# from test/runtests.jl as it is. Randomised tests use a small LCG, so they are
# reproducible without depending on Random.
#
# Known-open items are marked @test_broken: they document a defect and will turn
# into an "unexpectedly passing" error (the signal to delete the marker) once the
# defect is fixed.

using Test
using ModuleElts

# ---- deterministic pseudo-random integers -------------------------------------
mutable struct LCG
  s::UInt64
end
LCG(seed::Integer=20240101)=LCG(UInt64(seed))
function randint!(g::LCG,lo::Int,hi::Int)
  g.s=g.s*6364136223846793005+1442695040888963407
  g.s>>=1
  lo+Int(g.s%UInt64(hi-lo+1))
end

# ---- generic helpers valid for both implementations --------------------------
asdict(m)=Dict(first(p)=>last(p) for p in m)
iszerosafe(v)=iszero(v)

# the invariant both constructors maintain, and that every operation must preserve
function canonical(m::ModuleElt)
  ks=[first(p) for p in m]
  issorted(ks) && length(unique(ks))==length(ks) && !any(p->iszero(last(p)),m)
end
function canonical(m::HModuleElt)
  ks=[first(p) for p in m]
  length(unique(ks))==length(ks) && !any(p->iszero(last(p)),m)
end

# reference implementation of merge2: op on coefficients, zero for an absent key
function refmerge2(op,a,b)
  r=Dict{Symbol,Int}()
  for k in union(keys(asdict(a)),keys(asdict(b)))
    s=op(get(asdict(a),k,0),get(asdict(b),k,0))
    iszero(s) || (r[k]=s)
  end
  r
end

randel(g::LCG,M,n=8)=M([Symbol("k",i)=>randint!(g,-5,5) for i in 1:n if randint!(g,1,3)>1])

# is min/max implemented for this implementation? (merge2 for HModuleElt is new)
hasmerge2(M)=try
  hasmethod(ModuleElts.merge2,Tuple{Function,M{Symbol,Int},M{Symbol,Int}})
catch
  false
end

@testset "ModuleElts properties" verbose=true begin
  g=LCG()

  for M in (ModuleElt, HModuleElt)
    name=string(M)
    @testset "$name" begin

      @testset "constructor" begin
        a=M(:b=>2,:a=>1)
        @test canonical(a)
        @test asdict(a)==Dict(:a=>1,:b=>2)
        @test asdict(M([:b=>2,:a=>1]))==Dict(:a=>1,:b=>2)      # from a vector
        @test asdict(M([:b=>2,:a=>1]))==Dict(:a=>1,:b=>2)      # from a generator
        @test asdict(M(k=>1 for k in (:b,:a)))==Dict(:a=>1,:b=>1)
        @test iszero(zero(M{Symbol,Int}))                      # the empty element
        @test isempty(collect(zero(M{Symbol,Int})))
        # duplicate keys are summed, and a cancelling pair disappears
        @test asdict(M([:a=>1,:a=>2]))==Dict(:a=>3)
        @test asdict(M([:a=>1,:a=>-1]))==Dict{Symbol,Int}()
        @test canonical(M([:a=>1,:a=>-1]))
        # heterogeneous coefficient types are promoted
        @test valtype(M([:a=>1,:b=>1//2]))==Rational{Int}
      end

      @testset "arithmetic matches a Dict reference" begin
        for _ in 1:60
          a=randel(g,M); b=randel(g,M); c=randel(g,M)
          @test canonical(a)
          @test asdict(a+b)==refmerge2(+,a,b)
          @test asdict(a-b)==refmerge2(-,a,b)
          @test canonical(a+b) && canonical(a-b)
          @test a+b==b+a
          @test (a+b)+c==a+(b+c)
          @test a+(-a)==zero(a)
          @test -(-a)==a
          @test a-a==zero(a)
          @test a*3+a*4==a*7
          @test (a*2)*3==a*6
          @test asdict(a+a)==refmerge2(+,a,a)
          @test -a==M([k=>-v for (k,v) in a])
          @test iszero(a-a)
        end
      end

      @testset "AbstractDict interface" begin
        for _ in 1:30
          a=randel(g,M)
          d=asdict(a)
          for k in [Symbol("k",i) for i in 1:10]
            @test a[k]==get(d,k,0)          # getindex, including absent keys
            @test haskey(a,k)==haskey(d,k)   # haskey
            @test get(a,k,-99)==get(d,k,-99) # get with a default
          end
          @test length(a)==length(d)
          @test eltype(a)==Pair{Symbol,Int}
          @test Dict(collect(a))==d
          @test Dict(collect(pairs(a)))==d
          @test sort(collect(keys(a)))==sort(collect(keys(d)))
          @test length(collect(a))==length(d)
          if !isempty(a) @test a[first(first(a))]==first(last.(collect(a))) end
        end
      end

      @testset "setindex! and push!" begin
        for _ in 1:30
          a=randel(g,M)
          k=Symbol("zz")
          m=copy(a)
          m[k]=7
          @test m[k]==7 && canonical(m)
          m[k]=0                       # deleting by setting zero
          @test !haskey(m,k) && canonical(m)
          if !isempty(a)
            k2=first(first(a))
            m=copy(a); m[k2]=0
            @test !haskey(m,k2) && canonical(m)
            m=copy(a); m[k2]=last(first(a))+1
            @test m[k2]==last(first(a))+1 && canonical(m)
            m=copy(a); push!(m,k2=>-last(first(a)))   # cancels
            @test !haskey(m,k2) && canonical(m)
          end
        end
      end

      @testset "mergewith" begin
        # mergewith requires op(0,x)=op(x,0)=x: the coefficient of a key present
        # in only one argument is kept unchanged, not combined with zero
        for _ in 1:30
          a=randel(g,M); b=randel(g,M)
          @test asdict(mergewith(+,a,b))==refmerge2(+,a,b)
          @test canonical(mergewith(+,a,b))
          @test mergewith(+,a,b)==a+b
          # with an op that lacks the identity, single-side keys are kept as they
          # are: that is mergewith's documented requirement, not a defect
          x=M(:a=>3); y=M(:b=>4)
          @test asdict(mergewith(*,x,y))==Dict(:a=>3,:b=>4)
        end
        a=M(:a=>1,:b=>2); b=M(:b=>1,:c=>3)
        @test asdict(mergewith(+,a,b))==Dict(:a=>1,:b=>3,:c=>3)
        # '-' does not satisfy that identity, which is why merge2 is needed
        @test a-b!=mergewith(-,a,b)
        @test a-b==ModuleElts.merge2(-,a,b)
      end

      if hasmerge2(M)
        @testset "merge2, min, max" begin
          for _ in 1:60
            a=randel(g,M); b=randel(g,M)
            @test asdict(ModuleElts.merge2(+,a,b))==refmerge2(+,a,b)
            @test asdict(ModuleElts.merge2(-,a,b))==refmerge2(-,a,b)
            # an op that does not satisfy op(0,x)=op(x,0)=x, which is the whole
            # point of merge2: check both argument orders are respected
            op=(x,y)->2x+3y
            @test asdict(ModuleElts.merge2(op,a,b))==refmerge2(op,a,b)
            @test asdict(ModuleElts.merge2(op,b,a))==refmerge2(op,b,a)
            @test ModuleElts.merge2(op,a,b)!=ModuleElts.merge2(op,b,a) || isempty(a)!=isempty(b)
            @test asdict(min(a,b))==refmerge2(min,a,b)
            @test asdict(max(a,b))==refmerge2(max,a,b)
            @test canonical(min(a,b)) && canonical(max(a,b))
          end
          # min with an absorbing zero, spelled out
          @test isempty(min(M(:a=>1),M(:b=>2)))
          @test asdict(min(M(:a=>-1),M(:b=>2)))==Dict(:a=>-1)
          @test asdict(max(M(:a=>1),M(:b=>2)))==Dict(:a=>1,:b=>2)
          # '-' agrees with the dedicated implementation
          a=M(:a=>1,:b=>2); b=M(:b=>1,:c=>3)
          @test a-b==ModuleElts.merge2(-,a,b)
        end
      end

      @testset "convert and promotion" begin
        a=M(:a=>1,:b=>2)
        @test convert(M{Symbol,Int},a)===a || convert(M{Symbol,Int},a)==a
        r=convert(M{Symbol,Rational{Int}},a)
        @test asdict(r)==Dict(:a=>1//1,:b=>2//1)
        @test canonical(r)
        @test asdict(a+M(:c=>1//2))[Symbol("c")]==1//2
        @test valtype(a+M(:c=>1//2))==Rational{Int}
        # converting the key type sorts them for ModuleElt; contents must match
        ki=M(1=>1,2=>2)
        @test asdict(convert(M{Float64,Int},ki))==Dict(1.0=>1,2.0=>2)
      end

      @testset "equality, hash, sets and dict keys" begin
        for _ in 1:30
          a=randel(g,M)
          @test isequal(a,copy(a))
          @test hash(a)==hash(copy(a))
          @test length(Set([a,copy(a)]))==1
          @test unique([a,copy(a)])==[a]
          d=Dict(a=>:v)
          @test d[copy(a)]==:v
          @test haskey(d,copy(a))
        end
        # equal elements built in different orders hash the same. For HModuleElt
        # the iteration order of the Dict is not the insertion order, so this is
        # also the test that the hash does not depend on any ordering.
        @test hash(M(:a=>1,:b=>2))==hash(M(:b=>2,:a=>1))
        @test hash(M(:a=>1,:b=>2,:c=>3))==hash(M(:c=>3,:a=>1,:b=>2))
      end

    end
  end

  @testset "cross-implementation" begin
    a=ModuleElt(:a=>1,:b=>2)
    h=HModuleElt(:b=>2,:a=>1)
    @test a==h                       # Base's AbstractDict ==, needs get
    @test h==a
    @test isequal(a,h)
    @test a!=ModuleElt(:a=>1)
    @test hash(a)==hash(h)           # so Set/Dict work across implementations
    @test length(Set([a,h]))==1
    @test Dict(a=>1)[h]==1
    @test_throws ErrorException mergewith(+,a,h) # no promotion rule: explicit error
    @test_throws MethodError ModuleElts.merge2(+,a,h)
  end

  @testset "known-open items" begin
    # silent wraparound in normalize! is fixed for V<:Integer, but a Bool
    # coefficient type still cannot take a sum
    @test_broken ModuleElt(:a=>true)+ModuleElt(:a=>true)==ModuleElt(:a=>2)
    @test_broken HModuleElt(:a=>true)+HModuleElt(:a=>true)==HModuleElt(:a=>2)
    # convert does not renormalise, so a key collision can survive
    @test_broken length(convert(ModuleElt{Float64,Int},ModuleElt(2^53=>1,2^53+1=>2)))==1
    # ... and a coefficient can be rounded to zero
    @test_broken iszero(convert(ModuleElt{Symbol,Float32},ModuleElt(:a=>1e-50)))
  end
end
