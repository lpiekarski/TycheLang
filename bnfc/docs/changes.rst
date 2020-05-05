Release notes
*************

BNFC 2.7.0.0
============

* Add token support for Ocaml
* New option parser: the options now follow the standard convention of using two
  dashes for long options (e.g. ``--haskell``). Old options with a single dash (e.g.
  ``-haskell``) are still accepted but all newly introduced options use only the new
  format.
* Adds an optional argument to change Makefile name::

    bnfc -mMyMakefile

  should generate the Makefile in ``MyMakefile`` instead of the default
  (``Makefile``.) Add a long version of the option (``--makefile``)
* Add a ``--ghc`` option to derive Data, Typeable, Generic in the Haskell
  backend
* New online documentation (https://bnfc.readthedocs.org)
* Derive ``Read`` for newtype decls in Haskell backend.  E.g.
  ::

      newtype Ident = Ident String deriving (Eq,Ord,Read,Show)

  This allows users to translate ``Ident``'s from one language to another,
  e.g::

      LangX.IdentExp ident -> LangY.IdentExp (read (show ident))

* New option to get the version number: ``--version`` (the old one,
  ``--numeric-version`` still works).
* Remove the F# backend
* Remove the Java4 backend
* New Applicative and Alternative instances to ``Err``
* Remove the coupling between building the parser and the pdf from
  latex: this was causing a lot of confusion in the course because it
  would fail to build the test program if the student didn't have latex
  installed.  Add a separate target for the latex document (``--latex``).
* Improvement to the CNF Backend
* Bug fixes in different backends: Ocaml (#92, #21), GADT (#34, #33),
  Latex (#90), XML (#30), Java (#60)

Thanks to everyone who has contributed to this release: Adam Sandberg Eriksson,
Alex Rozenshteyn, Caleb Welton, Cubit, Grégoire Détrez, Jean-Philippe Bernardy,
Jeff Chen, John Lato, Josef Svenningsson, Oliver Bunting, Rob Stewart, Wictor
Lund, emptylambda, joe, runoshun
