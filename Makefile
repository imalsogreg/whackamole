all.js: whackamole_client/Main.hs
	cd whackamole_client && cabal configure --ghcjs && cabal build && cd ..
	cp whackamole_client/dist/build/whackamole-client/whackamole-client.jsexe/*.js ./static/
