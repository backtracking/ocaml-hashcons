(lang dune 2.7)

(name hashcons)

(license LGPL-2.1)
(maintainers "Jean-Christophe Filliatre <Jean-Christophe.Filliatre@lri.fr>")
(authors "Jean-Christophe Filliatre <Jean-Christophe.Filliatre@lri.fr>")

(source (github backtracking/ocaml-hashcons))

(generate_opam_files true)

(package
 (name hashcons)
 (synopsis "OCaml hash-consing library")
 (description "The technique is described in this paper:
*Sylvain Conchon and Jean-Christophe Filliâtre.* Type-Safe Modular Hash-Consing.
In ACM SIGPLAN Workshop on ML, Portland, Oregon, September 2006.
The PDF is available at
<https://www.lri.fr/~filliatr/ftp/publis/hash-consing2.pdf>")
 (depends (ocaml (>= 4.06))))
