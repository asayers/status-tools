status-tools: status-tools.hs
	ghc -O2 -dynamic status-tools.hs
	strip -s status-tools
