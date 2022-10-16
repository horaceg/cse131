(lang dune 3.4)

(name cse131)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name cse131)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune alcotest ocaml-lsp-server odig ocamlformat ocamlformat-rpc)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
