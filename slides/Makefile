BASENAME = hrdm
PDF = $(addsuffix .pdf, $(BASENAME))
TEX = $(addsuffix .tex, $(BASENAME))
PDFLATEX = pdflatex
OUT_DIR = texfiles

.PHONY: clean all

all: $(PDF)

$(PDF): $(TEX)
	@# Construct $(OUT_DIR) if it doesn't exist
	@test -d $(OUT_DIR) || mkdir $(OUT_DIR)
	@# Twice, so TOC is also updated
	@TEXINPUTS=$(STY_DIR)//: $(PDFLATEX) -output-directory $(OUT_DIR) $<
	@TEXINPUTS=$(STY_DIR)//: $(PDFLATEX) -output-directory $(OUT_DIR) $<
	@ln -sf $(OUT_DIR)/$@ .

clean:
	@$(RM) -r $(OUT_DIR)
	@$(RM) $(PDF)
