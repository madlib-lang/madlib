.text
.globl _madlib__stack__init
.p2align    2
_madlib__stack__init:
  stp    x29, x30, [sp, #-16]!
  mov    x29, sp

  mov sp, x0                        ; swap stack
  blr x1                            ; make the call
  mov sp, x29                       ; restore stack

  ldp     x29, x30, [sp], 16
  ret
