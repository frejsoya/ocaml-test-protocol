(lang dune 3.11)

(name ocaml_test_protocol)

(generate_opam_files true)

(source
 (github frejsoya/ocaml-test-protocol))

(authors "Frej Soya")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ocaml_test_protocol)
 (synopsis "Tap it")
 (description "A longer description")
 (depends ocaml dune angstrom ppx_inline_tests ppx_deriving)
 (tags
  (test tap harness)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
