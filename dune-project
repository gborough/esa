(lang dune 2.7)

(name esa)

(generate_opam_files true)

(source
 (github gborough/esa))

(authors "Geoffrey Borough <geoffrey.borough@outlook.com>")

(maintainers "Geoffrey Borough <geoffrey.borough@outlook.com>")

(license MIT)

(documentation https://gborough.github.io/esa/esa)

(package
 (name esa)
 (synopsis "Enhanced Suffix Array")
 (description
  "Pure OCaml implementation of Enhanced Suffix Array, with a focus on minimising allocations to stay close to the reference C++ implementation.")
 (depends
  (ocaml
   (>= 4.08.0))
  (ppx_expect :with-test))
 (tags
  (esa tokenizer llm bioinformatics)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
