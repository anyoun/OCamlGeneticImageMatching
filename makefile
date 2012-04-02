CC = clang
LIBBMP = /usr/local/lib/libbmp.a
OCAMLLIB = /opt/godi/lib/ocaml/std-lib 

all: ctest

ctest: genetic_match_fitness.c
	$(CC) $(LIBBMP) -I$(OCAMLLIB) genetic_match_fitness.c -o $@

ocaml: genetic_match_fitness.c genetic_match.ml
#	ocamlopt -verbose -ccopt -std=c99 -ccopt -O3 -ccopt -mfpmath=sse -ccopt -march=core2 -ccopt -I/usr/local/include/ImageMagick bigarray.cmxa -thread -I +libMagick magick.cmxa unix.cmxa threads.cmxa -I +lablGL lablgl.cmxa lablglut.cmxa -o ogim genetic_match_fitness.c genetic_match.ml
	ocamlopt -verbose -ccopt -std=c99 -ccopt -O3 -ccopt -mfpmath=sse -ccopt -march=core2 -ccopt $(LIBBMP) -thread -I unix.cmxa threads.cmxa -I +lablGL lablgl.cmxa lablglut.cmxa -o ogim genetic_match_fitness.c genetic_match.ml

clean:
	rm -f ctest
	rm -f ogim

.PHONY: clean