SOURCES = util.ml ast.ml parser.mly lexer.mll formula.mli formula.ml
RESULT = fm

all : byte-code-library native-code-library

OCAMLYACC=menhir

include OCamlMakefile
