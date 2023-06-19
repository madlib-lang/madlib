.text
.globl madlib__stack__init
.p2align    2
madlib__stack__init:
  stp    x29, x30, [sp, #-16]!
  mov    x29, sp

  mov sp, x0
  blr x1
  mov sp, x29

  ldp     x29, x30, [sp], 16
  ret