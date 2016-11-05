OUTDIR=build
OUT=$(OUTDIR)/index.html
SRC=Visual.elm
ELMC=elm-make
BROWSER=firefox
LIB=Data.elm

all: $(SRC) $(LIB) Makefile
	$(ELMC) $(SRC) --output $(OUT)

run: all
	$(BROWSER) $(OUT)

clean:
	rm $(OUTDIR)
