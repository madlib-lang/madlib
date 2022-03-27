  global    madlib__stack__init
  section   .text

; takes new stack address in rdi, and a function pointer in rsi
madlib__stack__init:
  mov     [rcx - 16], rsp ; store old stack address at the top of the new stack
  lea     rsp, [rcx - 16] ; set stack pointer just below the old stack address
  call    rdx             ; run your code
  pop     rsp             ; restore previous rsp
  ret                     ; return to outer code
