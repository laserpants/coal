#!/bin/bash

pdflatex -output-directory=tmp fold.tex
pdflatex -output-directory=tmp unfold.tex
pdflatex -output-directory=tmp product.tex
pdflatex -output-directory=tmp coproduct.tex

magick -density 300 tmp/fold.pdf -resize 50% png/fold.png
magick -density 300 tmp/unfold.pdf -resize 50% png/unfold.png
magick -density 300 tmp/product.pdf -resize 50% png/product.png
magick -density 300 tmp/coproduct.pdf -resize 50% png/coproduct.png
