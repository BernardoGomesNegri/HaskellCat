ghc ($args[0]) "-o" ("bin\" + $args[0] + ".exe")
Remove-Item ($args[0] + ".hi")
Remove-Item ($args[0] + ".o")