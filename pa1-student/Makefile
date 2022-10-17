UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  FORMAT=elf64
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho64
endif
endif
ifneq (,$(findstring Microsoft,$(shell cat /proc/version 2> /dev/null)))
  OCAMLOPT=-ocamlopt "ocamlopt -no-alias-deps"
endif

PKGS=oUnit,extlib,unix,sexplib
BUILD=ocamlbuild -r -use-ocamlfind -pkg $(PKGS) $(OCAMLOPT)

main: main.ml compile.ml runner.ml parser.ml
	$(BUILD) main.native
	mv main.native main

test: compile.ml runner.ml test.ml parser.ml myTests.ml
	$(BUILD) test.native
	mv test.native test

output/%.run: output/%.o main.c
	clang -g -o $@ main.c $<

output/%.o: output/%.s
	nasm -f $(FORMAT) -o $@ $<

output/%.s: input/%.ana main
	mkdir -p output
	./main $< > $@

clean:
	rm -rf output/*.o output/*.s output/*.dSYM output/*.run *.log
	rm -rf _build/
	rm -f main test
