FOURMOLU_EXTENSIONS := -o -XTypeApplications -o -XTemplateHaskell -o -XImportQualifiedPost -o -XPatternSynonyms

format:
	fourmolu $(FOURMOLU_EXTENSIONS) --mode inplace --check-idempotence $$(find src/ test/ -iregex ".*.hs")
	cabal-fmt -i $$(find . -iregex ".*.cabal")
	nixpkgs-fmt flake.nix
