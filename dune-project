(lang dune 2.7)
(name pratter)
(version 0.1)

(generate_opam_files true)
(license BSD-3-Clause)
(authors "Gabriel Hondet")
(maintainers "leirbag@sdfeu.org")
(source (github gabrielhdt/pratter))

(package
 (name pratter)
 (synopsis "An implementation of the Pratt parsing algorithm for first order terms.")
 (description "Pratter is a library that provides a parser that transforms
streams of terms to applied terms.  Terms may contain infix or prefix operators
and native applications.  The parser is an extension of the Pratt parsing
algorithm.")
 (depends (alcotest :with-test)))
