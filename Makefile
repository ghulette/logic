SOURCES = util.ml formula.ml parser.mly lexer.mll main.ml
RESULT = fm

all : byte-code native-code

include OCamlMakefile
