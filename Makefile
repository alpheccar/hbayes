all:
	ghc -prof -auto-all -cpp -DLOCAL -I~/Documents/Work/GITHUB/hbayes -o Main Main.hs

run:
	./Main +RTS -p 

runmem:
	./Main +RTS -p -hc

clean:
	rm -f *.o
	rm -f *.hi
	rm -f Main
