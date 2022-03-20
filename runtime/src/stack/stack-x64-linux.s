  global    _madlib__stack__init
  section   .text

; takes new stack address in rdi, and a function pointer in rsi
_madlib__stack__init:
  mov     [rdi - 16], rsp ; store old stack address at the top of the new stack
  lea     rsp, [rdi - 16] ; set stack pointer just below the old stack address
  call    rsi             ; run your code
  pop     rsp             ; restore previous rsp
  ret                     ; return to outer code
