default:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

check:
	bash checkenv.sh && bash checktypes.sh

clean:
	ocamlbuild -clean
	rm -f checktypes.ml

play:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte

test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

zip:
	zip ovegassrc.zip *.ml*
	
zipcheck:
	bash checkzip.sh
