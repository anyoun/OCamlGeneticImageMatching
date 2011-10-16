CC = clang
LIBBMP = /usr/local/lib/libbmp.a

all: ctest

ctest:
	$(CC) $(LIBBMP) genetic_match_fitness.c -o $@

clean:
	rm -f ctest

.PHONY: clean