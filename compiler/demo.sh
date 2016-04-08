#!/usr/bin/zsh

cat >demo.s <<PRE
INIT:
  ajw 1024

START:
PRE

bin/occ $1 -o - >> demo.s

cat >> demo.s <<POST
END:
POST
../vm/bin/vm <demo.s && rm demo.s
