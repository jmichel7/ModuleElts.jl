# auto-generated tests from julia-repl docstrings
using Test, ModuleElts
function mytest(file::String,cmd::String,man::String)
  println(file," ",cmd)
  exec=repr(MIME("text/plain"),eval(Meta.parse(cmd)),context=:limit=>true)
  if endswith(cmd,";") exec="nothing" 
  else exec=replace(exec,r"\s*$"m=>"")
       exec=replace(exec,r"\s*$"s=>"")
  end
  if exec!=man 
    i=1
    while i<=lastindex(exec) && i<=lastindex(man) && exec[i]==man[i]
      i=nextind(exec,i)
    end
    print("exec=$(repr(exec[i:end]))\nmanl=$(repr(man[i:end]))\n")
  end
  exec==man
end
@testset "ModuleElts.jl" begin
@test mytest("ModuleElts.jl","a=ModuleElt(:xy=>1,:yx=>-1)",":xy-:yx")
@test mytest("ModuleElts.jl","repr(a)","\"ModuleElt([:xy => 1, :yx => -1])\"")
@test mytest("ModuleElts.jl","ModuleElt([:xy=>1//2,:yx=>-1])","(1//2):xy+(-1//1):yx")
@test mytest("ModuleElts.jl","a-a","0")
@test mytest("ModuleElts.jl","a*99","99:xy-99:yx")
@test mytest("ModuleElts.jl","a//2","(1//2):xy+(-1//2):yx")
@test mytest("ModuleElts.jl","a/2","0.5:xy-0.5:yx")
@test mytest("ModuleElts.jl","a+ModuleElt(:yx=>1)",":xy")
@test mytest("ModuleElts.jl","a[:xy]","1")
@test mytest("ModuleElts.jl","a[:xx]","0")
@test mytest("ModuleElts.jl","haskey(a,:xx)","false")
@test mytest("ModuleElts.jl","a[:xx]=1","1")
@test mytest("ModuleElts.jl","first(a)",":xx => 1")
@test mytest("ModuleElts.jl","collect(a)","3-element Vector{Pair{Symbol, Int64}}:\n :xx => 1\n :xy => 1\n :yx => -1")
@test mytest("ModuleElts.jl","collect(keys(a))","3-element Vector{Symbol}:\n :xx\n :xy\n :yx")
@test mytest("ModuleElts.jl","collect(values(a))","3-element Vector{Int64}:\n  1\n  1\n -1")
@test mytest("ModuleElts.jl","length(a)","3")
@test mytest("ModuleElts.jl","eltype(a)","Pair{Symbol, Int64}")
@test mytest("ModuleElts.jl","a=ModuleElt(:yy=>1, :yx=>2, :xy=>3, :yy=>-1;check=false)",":yy+2:yx+3:xy-:yy")
@test mytest("ModuleElts.jl","a=ModuleElt(:yy=>1, :yx=>2, :xy=>3, :yy=>-1)","3:xy+2:yx")
@test mytest("ModuleElts.jl","a+ModuleElt([:z=>1.0])","3.0:xy+2.0:yx+1.0:z")
end
