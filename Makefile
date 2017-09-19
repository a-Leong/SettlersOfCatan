main:
	ocamlbuild -pkgs oUnit,str,ANSITerminal main.byte

play:
	ocamlbuild -pkgs oUnit,str,ANSITerminal main.byte && ./main.byte
	
test:
	ocamlbuild -pkgs oUnit,str,ANSITerminal ai_test.byte && ./ai_test.byte

clean:
	ocamlbuild -clean
	
