GHC = ghc -threaded -rtsopts

temp: example
	./example +RTS -N -sstderr

example: ui.o gl.o par.o example.o 
	$(GHC) -package priority-queue -package threads -package GLUtil -package OpenGL -package sdl2 $^ -o example

example.o: example.hs gl.hs ui.hs par.hs
ui.o: ui.hs
gl.o: gl.hs ui.hs
par.o: par.hs

#buddhabrot: buddhabrot.o
#	$(GHC) -package OpenGL -package GLUT $^ -o buddhabrot

clean:
	rm -Rf *.hi *.o buddhabrot example

%.o: %.hs
	$(GHC) -c $<
