(lang dune 3.12)
(cram enable)

(name serde_sexpr)

(generate_opam_files true)

(source
  (github serde-ml/sexpr))

(authors "Dillon Mulroy <dillon.mulroy@gmail.com>")

(maintainers "Dillon Mulroy <dillon.mulroy@gmail.com>" "Leandro Ostera <leandro@abstractmachines.dev>")

(license "MIT")

(package
  (name serde_sexpr)
  (synopsis "S-expression format support for Serde")
  (depends
    (ocaml (>= "4.14.0"))
    (serde (>= "0.0.2"))
    (serde_derive (and :with-test (>= "0.0.2")))
    (spices (and :with-test (>= "0.0.2")))
    (ppx_inline_test (and :with-test (>= "v0.16.0")))
    (sedlex (= 3.2))
    ))
