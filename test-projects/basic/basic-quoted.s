  .include "inc.i"
  .include cpp "inc-quoted.h"

  .org 0
  jmpf start

  ;; Header
  .org  $200

  .text 16 "Cheese Adventure"
  .text 32 "Cheese Adventure by Walter"
  .string 16 "waterbear"

  ;; Icon data
  .include icon "images/icon.gif" speed=16, eyecatch="images/eyecatch.png"

start:
  mov #VAL_01, ACC
  mov #VAL_02, ACC
  mov #VAL_03, ACC
  mov #VAL_04, ACC
  mov #VAL_05, ACC
  mov #VAL_06, ACC
  mov #VAL_07, ACC

from_c:
  mov #CVAL_01, ACC
  mov #CVAL_02, ACC
  mov #CVAL_03, ACC
  mov #CVAL_04, ACC
  mov #CVAL_05, ACC
  mov #CVAL_06, ACC
  mov #CVAL_07, ACC
  jmp from_c

.cnop 0, $200
