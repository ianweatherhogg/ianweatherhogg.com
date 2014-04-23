all: build

build: hakyll
	./hakyll build

rebuild: hakyll
	./hakyll rebuild

hakyll: hakyll.hs
	ghc --make hakyll.hs
	./hakyll clean

preview: hakyll
	./hakyll clean
	./hakyll preview -p 9000

clean: hakyll
	./hakyll clean
	rm -f hakyll

check: hakyll
	./hakyll check
