# Variables
OCAMLFIND = ocamlfind
OCAMLOPT = ocamlopt
PACKAGES = graphics
TARGET = main

# Modules
MODULES = helper graph
MLI_FILES = $(MODULES:=.mli)
ML_FILES = $(MODULES:=.ml)
CMI_FILES = $(MODULES:=.cmi)
CMO_FILES = $(MODULES:=.cmo)
CMX_FILES = $(MODULES:=.cmx)

# Rules
all: $(TARGET)

$(TARGET): $(CMX_FILES) $(TARGET).cmx
	$(OCAMLFIND) $(OCAMLOPT) -package $(PACKAGES) -linkpkg -o $(TARGET) $(CMX_FILES) $(TARGET).cmx

main.cmx: main.ml helper.cmi
	$(OCAMLOPT) -c $<

helper.cmx: helper.ml helper.cmi graph.cmi
	$(OCAMLFIND) $(OCAMLOPT) -package $(PACKAGES) -c $<

graph.cmx: graph.ml graph.cmi helper.cmi
	$(OCAMLOPT) -c $<

helper.cmi: helper.mli graph.cmi
	$(OCAMLOPT) -c $<

graph.cmi: graph.mli
	$(OCAMLOPT) -c $<

clean:
	rm -f $(TARGET) $(TARGET).cmx $(TARGET).cmi $(CMI_FILES) $(CMO_FILES) $(CMX_FILES) *.o *~

.PHONY: all clean

