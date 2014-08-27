temp: example
	./example

example: ui.o gl.o example.o 
	ghc -package GLUtil -package OpenGL -package sdl2 $^ -o example

example.o: example.hs gl.hs ui.hs
ui.o: ui.hs
gl.o: gl.hs ui.hs

run: buddhabrot
	./buddhabrot

buddhabrot: buddhabrot.o
	ghc -package OpenGL -package GLUT $^ -o buddhabrot

clean:
	rm -Rf *.hi *.o buddhabrot example

%.o: %.hs
	ghc -c $<
