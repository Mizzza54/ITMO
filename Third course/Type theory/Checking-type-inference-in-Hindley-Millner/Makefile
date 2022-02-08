COMPILER=ghc
OPTIONS=--make -i./src -o out/main

.PHONY: pack all run clean

all: out generate
	$(COMPILER) $(OPTIONS) app/Main.hs

out:
	mkdir -p out

generate:
	alex src/Lexer.x -o src/Lexer.hs
	happy src/Parser.y -o src/Parser.hs

run:
	./out/main

clean:
	rm -rf out

pack:
	zip checking.zip -r Makefile src/ app/
