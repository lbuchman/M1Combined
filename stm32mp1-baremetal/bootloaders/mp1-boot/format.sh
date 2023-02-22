#!/bin/sh

STYLE=java
OPTIONS_STYLE="--indent-preprocessor --verbose --indent-cases --indent-switches --indent-classes --break-elseifs --add-brackets --unpad-paren --convert-tabs --break-closing-brackets --break-blocks --indent-col1-comments --indent-switches --indent-classes --pad-oper"
astyle -n --recursive --style=${STYLE} ${OPTIONS_STYLE} "./*.cc"
astyle -n --recursive --style=${STYLE} ${OPTIONS_STYLE} "./*.cpp"
astyle -n --recursive --style=${STYLE} ${OPTIONS_STYLE} "./*.hh"
