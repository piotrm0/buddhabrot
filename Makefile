temp: example
	./example

example: example.o
	ghc -package OpenGL -package sdl2 $^ -o example

run: buddhabrot
	./buddhabrot

buddhabrot: buddhabrot.o
	ghc -package OpenGL -package GLUT $^ -o buddhabrot

clean:
	rm -Rf *.hi *.o buddhabrot

%.o: %.hs
	ghc -c $<
