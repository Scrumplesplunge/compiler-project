  ldc -1
  j FOO
BAR:
  adc 0xFFFF1234
  j BAR
FOO:
  ldc 0x4321
