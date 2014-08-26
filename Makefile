temp: example
	./example

example: ui.o example.o 
	ghc -package OpenGL -package sdl2 $^ -o example

example.o: example.hs ui.hs
ui.o: ui.hs

run: buddhabrot
	./buddhabrot

buddhabrot: buddhabrot.o
	ghc -package OpenGL -package GLUT $^ -o buddhabrot

clean:
	rm -Rf *.hi *.o buddhabrot

%.o: %.hs
	ghc -c $<
