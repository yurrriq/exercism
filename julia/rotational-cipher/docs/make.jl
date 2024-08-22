using Documenter, RotationalCipher
# using Documenter.Remotes: GitHub
DocMeta.setdocmeta!(RotationalCipher, :DocTestSetup, :(using RotationalCipher))


makedocs(
    sitename = "Rotational Cipher",
    remotes = nothing,
    # repo=GitHub("yurrriq", "exercism"),
    modules = [RotationalCipher],
    doctest = true,
)
