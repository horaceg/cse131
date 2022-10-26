output/%.run: output/%.o
	clang -g -mstackrealign -o $@ bin/main.c $<

output/%.o: output/%.s
	nasm -f elf64 -o $@ $<

output/%.s: input/%.boa compile
	mkdir -p output
	dune exec pa2 -- $< > $@

clean:
	rm -rf output/*.o output/*.s output/*.dSYM output/*.run *.log
	rm -rf _build/
