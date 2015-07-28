SOURCES = util.mli util.ml \
          valuation.mli valuation.ml \
          partial.mli partial.ml \
          ast.ml parser.mly lexer.mll \
          formula.mli formula.ml
RESULT = fm

all : byte-code-library native-code-library

OCAMLYACC = menhir
ANNOTATE = yes

include OCamlMakefile
