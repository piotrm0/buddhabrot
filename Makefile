GHC = ghc -threaded -rtsopts
SRCS = *.hs
TARGETS = mandel buddha

run: buddha
	./buddha +RTS -N -sstderr

# ui.o gl.o par.o example.o 
# -package priority-queue -package threads -package GLUtil -package OpenGL -package sdl2 $^ -o mandel
mandel: mandel.o
	$(GHC) --make mandel.hs

buddha: buddha.o
	$(GHC) --make buddha.hs

#buddhabrot: buddhabrot.o
#	$(GHC) -package OpenGL -package GLUT $^ -o buddhabrot

clean:
	rm -Rf *.hi *.o $(TARGETS) .dep

%.o %.hi: %.hs
	$(GHC) -c $<

include .dep

.dep: $(SRCS)
	$(GHC) -M -dep-makefile .dep $(SRCS)
