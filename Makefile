SOURCES = util.mli util.ml \
          valuation.mli valuation.ml \
          partial.mli partial.ml \
          parser.mli parser.ml \
          formula.mli formula.ml
RESULT = fm
PACKS = mparser.pcre

all : byte-code-library native-code-library

OCAMLYACC = menhir
ANNOTATE = yes

include OCamlMakefile
