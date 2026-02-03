(lang dune 3.0)

(using menhir 2.1)

(name coral66)

(version 0.1.0)

(authors "Zaneham")

(license Apache-2.0)

(source (github Zaneham/coral66-compiler))

(generate_opam_files true)

(package
 (name coral66)
 (synopsis "A modern CORAL 66 compiler for the 21st century")
 (description "Compiler for CORAL 66 (Computer On-line Real-time Applications Language), the British military and industrial real-time programming language. Used in RAF systems, Royal Navy vessels, nuclear power station control systems, and BT telephone exchanges. Because someone has to keep the reactors running.")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  (menhir (>= 20220210))
  (ppx_deriving (>= 5.0))
  (cmdliner (>= 1.1.0))))
