%.run: %.o
	clang -o $@ main.c $<

%.o: %.s
	nasm -f elf64 -o $@ $<
	# Use macho64 instead of elf64 for OSX

%.s: %.int
	dune exec cse131 -- $< > $@
