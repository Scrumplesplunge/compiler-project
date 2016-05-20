  # Stack space required: 4 word(s)
  # IF i = [0 FOR 6] ...
  ajw -2
  ldc 0
  stl 1
  ldc 6
  dup
  stl 2
  cj LOOP_END_3
REP_IF_2:
  # IF (message[BYTE i] = 111)
  # message[BYTE i]
  # i
  ldl 1
  # message
  mint
  adc 0
  add
  lb
  ldc 111
  # (message[BYTE i] = 111)
  diff
  eqc 0
  cj END_BRANCH_1
  # i
  ldl 1
  printdec
  ajw 2
  j END_IF_0
END_BRANCH_1:
  ldlp 1
  ldc LOOP_END_3 - REP_IF_2
  lend
LOOP_END_3:
  ajw 2
  stopp
END_IF_0:
  stopp
