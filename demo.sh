#!/usr/bin/zsh

cat >demo.s <<PRE
INIT:
  ajw 256

START:
PRE

bin/Parser <$1 >> demo.s

cat >> demo.s <<POST
END:
POST
../vm/bin/vm <demo.s
rm demo.s
