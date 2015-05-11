Langname := adora
ParserHsPrefixes := Abs Print Par Lex Layout Test
ParserHs := $(addsuffix $(Langname).hs,$(ParserHsPrefixes))
ParserObjects := $(patsubst %.hs,%.o,$(ParserHs))
OtherHs := $(filter-out $(ParserHs),$(wildcard *.hs))
BnfcFiles := $(subst __,$(Langname),Lex__.x Par__.y Doc__.tex Abs__.hs Print__.hs Layout__.hs Test__.hs)
Examples := $(wildcard good/*.adora) $(wildcard bad/*.adora)

all: interpreter

interpreter: $(ParserObjects) $(OtherHs)
	@#ghc -Wall -Werror --make interpreter.hs -o interpreter
	ghc -Wall --make interpreter.hs -o interpreter

Testadora: $(ParserObjects)
	ghc --make Testadora.hs -o Testadora

$(ParserObjects): %.o: %.hs $(ParserHs)
	@# bnfc generated files  - compiled without -Wall and -Werror
	ghc --make "$<"

Lexadora.hs: Lexadora.x
	alex -g Lexadora.x

Paradora.hs: Paradora.y
	happy -gcai Paradora.y

Docadora.ps: Docadora.tex
	latex Docadora.tex; dvips Docadora.dvi -o Docadora.ps

adora.pdf: adora.html
	wkhtmltopdf --title 'Adora README (Franciszek Boehlke)' adora.html adora.pdf

adora.html: adora.md
	pandoc -t markdown -t html -s -S "$^" -o "$@" --css=github.css --self-contained --highlight-style=kate

$(BnfcFiles): $(Langname).cf
	bnfc -haskell adora.cf
	touch $(BnfcFiles)  # let's make know these files are up to date

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi *.x *.y
	-rm -f Docadora.ps adora.html adora.pdf
	-rm -rf franciszek_boehlke franciszek_boehlke.zip

distclean: clean
	-rm -f Docadora.* Lexadora.* Paradora.* Layoutadora.* Skeladora.* Printadora.* Testadora.* Absadora.* Testadora ErrM.* SharedString.* adora.dtd XMLadora.*

franciszek_boehlke.zip: $(patsubst %,franciszek_boehlke/%,README.pdf adora.cf Makefile $(Examples) $(OtherHs))
	tar -lzip -cf franciszek_boehlke.zip franciszek_boehlke

franciszek_boehlke/README.pdf: adora.pdf
	mkdir -p $$(dirname "$@")
	cp "$<" "$@"

franciszek_boehlke/%: %
	mkdir -p $$(dirname "$@")
	cp "$<" "$@"
