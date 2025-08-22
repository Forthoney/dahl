(lang dune 3.19)

(name dahl)

(using menhir 3.0)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Seong-Heon Jung <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name dahl)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
