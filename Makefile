Langname := adora
ParserHsPrefixes := Abs Layout Lex Par Print Skel
ParserHs := $(addsuffix $(Langname).hs,$(ParserHsPrefixes))
ParserObjects := $(patsubst %.hs,%.o,$(ParserHs))
SourceHs := $(filter-out $(ParserHs) StdLib.hs Testadora.hs,$(wildcard *.hs))
BnfcFiles := $(subst __,$(Langname),Lex__.x Par__.y Doc__.tex Abs__.hs Print__.hs Layout__.hs Test__.hs Skel__.hs)
Examples := $(wildcard good/*.adora) $(wildcard bad/*.adora)
CommonGHCFlags := -cpp -O2 -DUSE_HASKELINE

all: Testadora interpreter

%: %.hs
	ghc $(CommonGHCFlags) -Wall -Werror --make "$<" -o "$@"

interpreter: $(ParserObjects) $(filter-out parselib.hs,$(SourceHs)) StdLib.hs
parselib: $(ParserObjects) $(SourceHs) $(filter-out interpreter.hs,$(SourceHs))

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

Docadora.ps: Docadora.tex
	latex Docadora.tex; dvips Docadora.dvi -o Docadora.ps

adora.pdf: adora.html
	wkhtmltopdf --title 'Adora README (Franciszek Boehlke)' adora.html adora.pdf

adora.html: adora.md
	pandoc -s -S "$^" -o "$@" --css=github.css --self-contained --highlight-style=kate

$(BnfcFiles): $(Langname).cf
	bnfc -haskell adora.cf >/dev/null
	touch $(BnfcFiles)  # tell `make` these files are up to date

franciszek_boehlke.zip: $(patsubst %,franciszek_boehlke/%,README.pdf adora.cf stdlib.adora Makefile $(Examples) $(SourceHs))
	tar -acf franciszek_boehlke.zip franciszek_boehlke

franciszek_boehlke/README.pdf: adora.pdf
	mkdir -p $$(dirname "$@")
	cp "$<" "$@"

franciszek_boehlke/Makefile: Makefile
	mkdir -p $$(dirname "$@")
	sed 's/-D''USE_HASKELINE//g' "$<" > "$@"

franciszek_boehlke/%: %
	mkdir -p $$(dirname "$@")
	cp "$<" "$@"

.PHONY: test_students

test_students: franciszek_boehlke.zip students_test_script.sh
	scp franciszek_boehlke.zip fb320589@students.mimuw.edu.pl:~/jpp/adora
	ssh fb320589@students.mimuw.edu.pl < students_test_script.sh

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi *.x *.y
	-rm -f Docadora.ps adora.html adora.pdf
	-rm -rf parselib StdLib.hs
	-rm -rf franciszek_boehlke franciszek_boehlke.zip

distclean: clean
	-rm -f Docadora.* Lexadora.* Paradora.* Layoutadora.* Skeladora.* Printadora.* Testadora.* Absadora.* Testadora ErrM.* SharedString.* adora.dtd XMLadora.*
