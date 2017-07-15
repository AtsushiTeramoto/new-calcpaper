#!/bin/sh
new-calcpaper-exe
dockertexlive "ptex2pdf -l -ot '-synctex=1' out.tex"
./print.sh
