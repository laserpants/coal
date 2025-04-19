#!/bin/bash

TMP_DIR="tmp"

process_image () {
    TEX_FILE="tex/$1.tex"
    DVI_FILE="$TMP_DIR/$1.dvi"
    latex -output-directory="tmp" $TEX_FILE
    dvipng -T tight -D 300 -bg White -o "$2" $DVI_FILE
    magick "$2" -resize 450x450 "$2"
}

mkdir -p tmp
# process_image "expr_grammar" "./expr_grammar.png"
process_image "type" "./type.png"

# pdflatex -output-directory="$TMP_DIR" "tex/overview.tex"
# magick -density 600 "$TMP_DIR/overview.pdf" -trim -background white -alpha remove -bordercolor White -border 40 "./overview.png"
# magick overview.png -resize 400x400 overview.png
# 
# pdflatex -output-directory="$TMP_DIR" "tex/ast1.tex"
# magick -density 600 "$TMP_DIR/ast1.pdf" -trim -background white -alpha remove -bordercolor White -border 40 "./ast1.png"
# magick ast1.png -resize 800x800 ast1.png
