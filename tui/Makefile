build/%:
	cabal build tui-$(notdir $@)

v%: build/v%
	clear
	cabal run tui-$@

