Langname := adora
ParserHsPrefixes := Abs Layout Lex Par Print Skel
ParserHs := $(addsuffix $(Langname).hs,$(ParserHsPrefixes))
ParserObjects := $(patsubst %.hs,%.o,$(ParserHs))
SourceHs := $(filter-out $(ParserHs) StdLib.hs Testadora.hs,$(wildcard *.hs))
BnfcFiles := $(subst __,$(Langname),Lex__.x Par__.y Doc__.tex Doc__.txt Abs__.hs Print__.hs Layout__.hs Test__.hs Skel__.hs)
GoodExamples := $(wildcard good/*.adora)
BadExamples := $(wildcard bad/*.adora)
Examples := $(GoodExamples) $(BadExamples)
CommonGHCFlags := -cpp -O2 -DUSE_HASKELINE

all: Testadora interpreter

%: %.hs
	ghc $(CommonGHCFlags) -Wall -Werror --make "$<" -o "$@"

interpreter: $(ParserObjects) $(filter-out parselib.hs,$(SourceHs)) StdLib.hs
parselib: $(ParserObjects)

Testadora: $(ParserObjects) Testadora.hs
	# special rule without -Wall and -Werror
	ghc $(CommonGHCFlags) --make Testadora.hs -o Testadora

StdLib.hs: stdlib.adora parselib
	./parselib stdlib.adora StdLib.hs stdlib

$(ParserObjects): %.o: %.hs $(ParserHs)
	@# bnfc generated files  - compiled without -Wall and -Werror
	ghc $(CommonGHCFlags) -w --make "$<"

Lexadora.hs: Lexadora.x
	alex -g Lexadora.x

Paradora.hs: Paradora.y
	happy -gcai Paradora.y

Docadora.pdf: %.pdf: %.tex
	- pdflatex "$<"

adora.pdf: %.pdf: %.html
	wkhtmltopdf --title 'Adora README (Franciszek Boehlke)' "$<" "$@"

adora.html: adora.md github.css
	pandoc -s -S "$<" -o "$@" --css=github.css --self-contained --highlight-style=kate

$(BnfcFiles): $(Langname).cf
	bnfc -haskell adora.cf >/dev/null
	touch $(BnfcFiles)  # tell `make` these files are up to date

define ZipFiles
README.pdf adora.cf stdlib.adora\
run-test.sh run-test-bad.sh\
Makefile $(Examples) $(SourceHs)
endef

franciszek_boehlke.tar.gz: $(patsubst %,franciszek_boehlke/%,$(ZipFiles))
	tar -acf franciszek_boehlke.tar.gz franciszek_boehlke

franciszek_boehlke/README.pdf: adora.pdf
	mkdir -p $$(dirname "$@")
	cp "$<" "$@"

franciszek_boehlke/Makefile: Makefile
	mkdir -p $$(dirname "$@")
	sed 's/-D''USE_HASKELINE//g' "$<" > "$@"

franciszek_boehlke/%: %
	mkdir -p $$(dirname "$@")
	cp "$<" "$@"

GoodTestTargets := $(addprefix test-target-,$(GoodExamples))
BadTestTargets := $(addprefix test-target-,$(BadExamples))
TestTargets := $(GoodTestTargets) $(BadTestTargets)
.PHONY: test-students test-good $(TestTargets)

test-students: franciszek_boehlke.tar.gz students-test-script.sh
	scp franciszek_boehlke.tar.gz fb320589@students.mimuw.edu.pl:~/jpp/adora
	ssh fb320589@students.mimuw.edu.pl < students-test-script.sh

test: $(TestTargets)

$(GoodTestTargets): test-target-%: % interpreter
	@./run-test.sh "$<"

$(BadTestTargets): test-target-%: % interpreter
	@./run-test-bad.sh "$<"

clean:
	-rm -f *.log *.aux *.hi *.o *.ps *.dvi *.x *.y
	-rm -f adora.html adora.pdf Docadora.pdf
	-rm -f parselib StdLib.hs
	-rm -rf franciszek_boehlke

distclean: clean
	-rm -f Docadora.* Lexadora.* Paradora.* Layoutadora.* Skeladora.* Printadora.* Testadora.* Absadora.* ErrM.* SharedString.* adora.dtd XMLadora.*
	-rm -f interpreter Testadora
	-rm -rf franciszek_boehlke.tar.gz
