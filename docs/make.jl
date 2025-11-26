using Documenter, ModuleElts

DocMeta.setdocmeta!(ModuleElts, :DocTestSetup, :(using ModuleElts); recursive=true)

makedocs(;
    modules=[ModuleElts],
    authors="Jean Michel <jean.michel@imj-prg.fr> and contributors",
    sitename="ModuleElts.jl",
    format=Documenter.HTML(;
        canonical="https://jmichel7.github.io/ModuleElts.jl",
        edit_link="main",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
    warnonly=:missing_docs,
)

deploydocs(;
    repo="github.com/jmichel7/ModuleElts.jl",
    devbranch="main",
)
