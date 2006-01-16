all: extract_args

extract_args: extract_args.ml
	ocamlc -o extract_args -custom str.cma extract_args.ml

clean:
	rm -f *.cmo *.cmi *.cma extract_args
