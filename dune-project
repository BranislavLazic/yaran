(lang dune 3.6)

(name yaran)

(generate_opam_files true)

(source
 (github BranislavLazic/yaran))

(authors "Branislav Lazic")

(maintainers "Branislav Lazic")

(license LICENSE)

(package
 (name yaran)
 (synopsis "A Lisp like language")
 (description "A Lisp like language")
 (depends
  ocaml
  dune
  ppx_deriving
  (alcotest
   (>= 1.7.0))))
