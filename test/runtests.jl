# auto-generated tests from julia-repl docstrings
using Test, ModuleElts
function mytest(a::String,b::String)
  omit=a[end]==';'
  a=replace(a,"\\\\"=>"\\")
  a=repr(MIME("text/plain"),eval(Meta.parse(a)),context=:limit=>true)
  if omit a="nothing" end
  a=replace(a,r" *(\n|$)"s=>s"\1")
  a=replace(a,r"\n$"s=>"")
  b=replace(b,r" *(\n|$)"s=>s"\1")
  b=replace(b,r"\n$"s=>"")
  i=1
  while i<=lastindex(a) && i<=lastindex(b) && a[i]==b[i]
    i=nextind(a,i)
  end
  if a!=b print("exec=$(repr(a[i:end]))\nmanl=$(repr(b[i:end]))\n") end
  a==b
end
@testset verbose = true "Gapjm" begin
@testset "README.md" begin
@test mytest("a=ModuleElt(:xy=>1,:yx=>-1)",":xy-:yx")
@test mytest("repr(a)","\"ModuleElt([:xy => 1, :yx => -1])\"")
@test mytest("a-a","0")
@test mytest("a*99","99:xy-99:yx")
@test mytest("a//2","(1//2):xy+(-1//2):yx")
@test mytest("a/2","0.5:xy-0.5:yx")
@test mytest("a+ModuleElt(:yx=>1)",":xy")
@test mytest("a[:xy]","1")
@test mytest("a[:xx]","0")
@test mytest("haskey(a,:xx)","false")
@test mytest("first(a)",":xy => 1")
@test mytest("collect(a)","2-element Vector{Pair{Symbol, Int64}}:\n :xy => 1\n :yx => -1")
@test mytest("collect(keys(a))","2-element Vector{Symbol}:\n :xy\n :yx")
@test mytest("collect(values(a))","2-element Vector{Int64}:\n  1\n -1")
@test mytest("length(a)","2")
@test mytest("eltype(a)","Pair{Symbol, Int64}")
@test mytest("a=ModuleElt(:yy=>1, :yx=>2, :xy=>3, :yy=>-1;check=false)",":yy+2:yx+3:xy-:yy")
@test mytest("a=ModuleElt(:yy=>1, :yx=>2, :xy=>3, :yy=>-1)","3:xy+2:yx")
@test mytest("a+ModuleElt([:z=>1.0])","3.0:xy+2.0:yx+1.0:z")
end
end
