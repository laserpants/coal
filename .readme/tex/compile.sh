#!/bin/bash

mkdir -p tmp

pdflatex -output-directory=tmp fold.tex
pdflatex -output-directory=tmp unfold.tex
pdflatex -output-directory=tmp fold_unfold.tex
pdflatex -output-directory=tmp product_coproduct.tex
pdflatex -output-directory=tmp fixed_point.tex
pdflatex -output-directory=tmp pattern_functor.tex

magick -density 300 tmp/fold.pdf -resize 50% -bordercolor white -border 10x10 png/fold.png
magick -density 300 tmp/unfold.pdf -resize 50% -bordercolor white -border 10x10 png/unfold.png
magick -density 300 tmp/fold_unfold.pdf -resize 40% -bordercolor white -border 10x10 png/fold_unfold.png
magick -density 300 tmp/product_coproduct.pdf -resize 40% -bordercolor white -border 10x10 png/product_coproduct.png
magick -density 300 tmp/fixed_point.pdf -resize 50% -bordercolor white -border 10x10 png/fixed_point.png
magick -density 300 tmp/pattern_functor.pdf -resize 50% -bordercolor white -border 10x10 png/pattern_functor.png
