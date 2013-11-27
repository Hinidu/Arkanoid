all:
	ghc --make -O2 Arkanoid.hs

clean:
	rm *.o *.hi Arkanoid
