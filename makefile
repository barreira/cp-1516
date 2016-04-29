prog:
	ghc -fhpc Test.hs
	./Test
	hpc markup Test --exclude=Main

clean:
	rm -f *.hi
	rm -f *.html
	rm -f *.o
	rm -f *.tix
	rm -f Test
