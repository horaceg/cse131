compile:
	dune build

output/%.run: output/%.o
	clang -g -fsanitize=address -o $@ bin/main.c $<

exec: output/%.cwasm
	wasmtime run --allow-precompiled $<

output/%.cwasm: output/%.wasm
	wasmtime compile -o $@ $<

output/%.wasm: input/%.ana compile
	mkdir -p output
	dune exec pa1 -- $< > $@

clean:
	rm -rf output/*.o output/*.s output/*.dSYM output/*.run *.log
	rm -rf _build/
