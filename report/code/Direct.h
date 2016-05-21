// AUTOMATICALLY GENERATED CODE - DO NOT EDIT.

#pragma once

#include <string>

enum Direct {
  J     = 0,    // Jump
  LDLP  = 16,   // Load local pointer
  PFIX  = 32,   // Prefix
  LDNL  = 48,   // Load non-local
  LDC   = 64,   // Load constant
  LDNLP = 80,   // Load non-local pointer
  NFIX  = 96,   // Negative prefix
  LDL   = 112,  // Load local
  ADC   = 128,  // Add constant
  CALL  = 144,  // Call subroutine
  CJ    = 160,  // Conditional jump
  AJW   = 176,  // Adjust workspace
  EQC   = 192,  // Equals constant
  STL   = 208,  // Store local
  STNL  = 224,  // Store non-local
  OPR   = 240,  // Operate
};

std::string toString(Direct value);
bool fromString(const std::string& value, Direct* output);

