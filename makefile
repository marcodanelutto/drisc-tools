SOURCES = asm.ml prettyprinter.ml lexer.mll parser.mly util.ml interp.ml parse.ml assembler.ml

all: 	parse drisc

parser: 
	ocamlc asm.ml
	ocamlc prettyprinter.ml
	ocamllex lexer.mll       # generates lexer.ml
	ocamlyacc parser.mly     # generates parser.ml and parser.mli
	ocamlc -c parser.mli

parse:	parse.ml parser
	ocamlc -o parse asm.ml prettyprinter.ml lexer.ml parser.ml util.ml interp.ml parse.ml

drisc: 	parser assembler.ml
	ocamlc -o drisc asm.ml prettyprinter.ml lexer.ml parser.ml util.ml interp.ml assembler.ml

clean: 
	rm -f *cmi *cmo parser.ml lexer.ml *mli drisc a.out parse

tar:
	tar czvf drisc.tgz --exclude */._* $(SOURCES) SamplePrograms makefile README README.changes

rsync: 
	rsync -avz * marcod@backus.di.unipi.it:Drisc/
