#!/bin/bash

pdflatex -output-directory=tmp fold_unfold.tex
pdflatex -output-directory=tmp product_coproduct.tex

magick -density 300 tmp/fold_unfold.pdf -resize 50% png/fold_unfold.png
magick -density 300 tmp/product_coproduct.pdf -resize 50% png/product_coproduct.png
