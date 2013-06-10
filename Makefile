status-tools: status-tools.hs
	ghc -O2 -dynamic status-tools.hs
	strip -s status-tools

battery: battery.hs
	ghc -O2 -dynamic battery.hs
	strip -s battery

network: network.hs
	ghc -O2 -dynamic network.hs
	strip -s network

volume: volume.hs
	ghc -O2 -dynamic volume.hs
	strip -s volume
