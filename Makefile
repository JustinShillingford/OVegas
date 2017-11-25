default:
	ocamlbuild -use-ocamlfind -pkg oUnit2 test.byte

check:
	bash checkenv.sh && bash checktypes.sh

clean:
	ocamlbuild -clean
	rm -f checktypes.ml

play:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte