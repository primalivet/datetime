.PHONY: switch start
.PHONY: test

switch: # Create OPAM switch and install dependencies
	opam switch create . 5.0.0 --no-install --repos pin_datetime=git+https://github.com/ocaml/opam-repository#f34983a7f8c448578addc5ec02f9c22d2b32650c
	opam install -y ocamlformat=0.26.1 ocaml-lsp-server=1.17.0
	opam install -y --deps-only --with-test --with-doc .
build: # Build project
	opam exec -- dune build

test: # Test project
	opam exec -- dune runtest --force
test-watch: # Test project
	opam exec -- dune runtest --force --watch

watch: # Build project and rebuild upon change in source files
	opam exec -- dune build --watch
