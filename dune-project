(lang dune 3.7)

(name datetime)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name datetime)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
   (ocaml (>= 5.0.0)) 
   unix
   (base (>= 0.16.3))
   (alcotest :with-test)
   dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
