.RECIPEPREFIX = >
DISABLED_RULES=WHITESPACE_RULE,COMMA_PARENTHESIS_WHITESPACE,CURRENCY,EN_QUOTES,CURRENCY_SPACE,ENGLISH_WORD_REPEAT_BEGINNING_RULE

proj = Thesis

out_dir = out/
fig_dir = figures/
src_dir = src/
res_dir = result/

pdflatex_args = -halt-on-error -shell-escape

input = $(src_dir)/$(proj).tex
output = $(out_dir)/$(proj).pdf

latexmk = latexmk -xelatex -halt-on-error -shell-escape -outdir=../$(out_dir) -cd

latexmk_sing = $(latexmk) $(input)
latexmk_cont = $(latexmk) -pvc $(input)

all: $(out_dir)
> $(latexmk_sing)
> rmdir ../out
#> make clean

continuous: $(out_dir)
> $(latexmk_cont); make full-clean
> rmdir ../out

$(out_dir):
> -if [ ! -d $(out_dir) ]; then mkdir -p $(out_dir); fi
> cp -R $(fig_dir) $(out_dir)/

clean:
> -pkill evince || true
> -if [ ! -d $(res_dir) ]; then mkdir $(res_dir); fi
> -cp -p $(output) $(res_dir) &>/dev/null || true
> -if [ ! -d $(out_dir) ]; then mkdir $(out_dir); fi
> rm -I -rf $(out_dir)/*
> rmdir $(out_dir)

full-clean: clean
> rm -I -f $(res_dir)/*
> rmdir $(res_dir)

test:
> languagetool -d $(DISABLED_RULES) $(orginput)
