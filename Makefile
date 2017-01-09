OCAMLBUILD = ocamlbuild

SOURCES = r2.ml test.ml main.ml
RESULT = main.native
OCAMLLDFLAGS = -g

all: $(SOURCES)
	$(OCAMLBUILD) $(RESULT) -pkgs oUnit -pkgs str -pp "camlp4o pa_extend.cmo" -I +camlp4 

clean:
	$(OCAMLBUILD) -clean
