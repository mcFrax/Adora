Langname := adora
ParserHsPrefixes := Abs Layout Lex Par Print Skel
ParserHs := $(addsuffix $(Langname).hs,$(ParserHsPrefixes)) ErrM.hs
ParserObjects := $(patsubst %.hs,%.o,$(ParserHs))
SourceHs := $(filter-out $(ParserHs) StdLib.hs Testadora.hs,$(wildcard *.hs))
BnfcFiles := $(subst __,$(Langname),Lex__.x Par__.y Doc__.tex Doc__.txt Abs__.hs Print__.hs Layout__.hs Test__.hs Skel__.hs ErrM.hs)
GoodExamples := $(wildcard good/*.adora)
BadExamples := $(wildcard bad/*.adora)
Examples := $(GoodExamples) $(BadExamples)
TestBuild :=
TestBuildMarker := ./testbuild
OptBuildMarker := ./optbuild
BuildMarker := $(if $(TestBuild),$(TestBuildMarker),$(OptBuildMarker))
CommonGHCFlags := -cpp $(if $(TestBuild),-fhpc -prof -auto-all -caf-all,-O2) -DUSE_HASKELINE

all: Testadora interpreter

%: %.hs $(BuildMarker)
	ghc $(CommonGHCFlags) -Wall -Werror --make "$<" -o "$@"

interpreter: $(ParserObjects) $(filter-out parselib.hs,$(SourceHs)) StdLib.hs
parselib: $(ParserObjects)

Testadora: $(ParserObjects) Testadora.hs
	# special rule without -Wall and -Werror
	ghc $(CommonGHCFlags) --make Testadora.hs -o Testadora

StdLib.hs: stdlib.adora parselib
	./parselib stdlib.adora StdLib.hs stdlib

$(ParserObjects): %.o: %.hs $(ParserHs) $(BuildMarker)
	@# bnfc generated files  - compiled without -Wall and -Werror
	ghc $(CommonGHCFlags) -w --make "$<"

Lexadora.hs: Lexadora.x
	alex -g Lexadora.x

Paradora.hs: Paradora.y
	happy -gcai Paradora.y

Docadora.pdf: %.pdf: %.tex
	- pdflatex "$<"

readme.pdf: %.pdf: %.html
	wkhtmltopdf --title 'Adora language' "$<" "$@"

readme.html: readme.md github.css
	pandoc -s -S "$<" -o "$@" --css=github.css --self-contained --highlight-style=kate

$(BnfcFiles): $(Langname).cf
	bnfc -haskell adora.cf >/dev/null || bnfc -haskell adora.cf 1>&2
	touch $(BnfcFiles)  # tell `make` these files are up to date

GoodTestCases := $(addprefix test-case-,$(GoodExamples))
BadTestCases := $(addprefix test-case-,$(BadExamples))
TestCases := $(GoodTestCases) $(BadTestCases)
.PHONY: test $(TestCases) test-coverage

test: interpreter
	@./run-test.py $(GoodExamples) $(BadExamples)

define CollectCoverage
mkdir -p coverage;
hpc markup interpreter.tix --destdir=coverage $(patsubst %.hs,--exclude=%,$(ParserHs))
endef

test-coverage: $(if $(TestBuild),tix-clean test,)
	$(if $(TestBuild),$(CollectCoverage),$(MAKE) TestBuild=1 test-coverage)

$(GoodTestCases): test-case-%: % interpreter
	@./run-test.py "$<"

$(BadTestCases): test-case-%: % interpreter
	@./run-test.py "$<"


$(TestBuildMarker):
	if [ -f "$(OptBuildMarker)" ]; then $(MAKE) clean; fi
	touch $(TestBuildMarker)

$(OptBuildMarker):
	if [ -f "$(TestBuildMarker)" ]; then $(MAKE) clean; fi
	touch $(OptBuildMarker)

tix-clean:
	-rm -f *.tix

clean: tix-clean
	-rm -f *.log *.aux *.hi *.o *.ps *.dvi *.x *.y
	-rm -f readme.html readme.pdf Docadora.pdf
	-rm -f parselib StdLib.hs
	-rm -f $(TestBuildMarker) $(OptBuildMarker)

distclean: clean
	-rm -f Docadora.* Lexadora.* Paradora.* Layoutadora.* Skeladora.* Printadora.* Testadora.* Absadora.* ErrM.* SharedString.* adora.dtd XMLadora.*
	-rm -f coverage/*
	-rm -f interpreter Testadora
