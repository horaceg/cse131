compile:
	dune build

output/%.run: output/%.o
	clang -g -fsanitize=address -o $@ bin/main.c $<

output/%.o: output/%.s
	nasm -f elf64 -o $@ $<

output/%.s: input/%.ana compile
	mkdir -p output
	dune exec pa1 -- $< > $@

clean:
	rm -rf output/*.o output/*.s output/*.dSYM output/*.run *.log
	rm -rf _build/
