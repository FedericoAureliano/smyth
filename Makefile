build:
	dune build src/main.exe

serve:
	make build && python3 serve.py

repl:
	dune utop lib/smyth

clean:
	dune clean

deps:
	opam install \
		utop dune yojson ppx_deriving ppx_deriving_yojson bark
