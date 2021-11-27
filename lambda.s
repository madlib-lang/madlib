	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 15
	.globl	_$Show$Number$show      ## -- Begin function $Show$Number$show
	.p2align	4, 0x90
_$Show$Number$show:                     ## @"$Show$Number$show"
	.cfi_startproc
## %bb.0:
	leaq	_showNumber(%rip), %rax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_$Functor$List$map$fn   ## -- Begin function $Functor$List$map$fn
	.p2align	4, 0x90
_$Functor$List$map$fn:                  ## @"$Functor$List$map$fn"
	.cfi_startproc
## %bb.0:
	pushq	%r14
	.cfi_def_cfa_offset 16
	pushq	%rbx
	.cfi_def_cfa_offset 24
	pushq	%rax
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -24
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movl	$8, %edi
	callq	_GC_malloc
	movq	%rax, %rbx
	movq	%r14, (%rax)
	movl	$16, %edi
	callq	_GC_malloc
	leaq	_$closureFn$2(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rbx, 8(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_Just$0                 ## -- Begin function Just$0
	.p2align	4, 0x90
_Just$0:                                ## @"Just$0"
	.cfi_startproc
## %bb.0:
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movq	%rsi, %rbx
	movl	$8, %edi
	callq	_GC_malloc
	movq	%rbx, (%rax)
	movl	$16, %edi
	callq	_GC_malloc
	movq	%rbx, 8(%rax)
	movq	$0, (%rax)
	popq	%rbx
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__literal8,8byte_literals
	.p2align	3               ## -- Begin function $closureFn$0
LCPI3_0:
	.quad	4607182418800017408     ## double 1
LCPI3_1:
	.quad	-4616189618054758400    ## double -1
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_$closureFn$0
	.p2align	4, 0x90
_$closureFn$0:                          ## @"$closureFn$0"
	.cfi_startproc
## %bb.0:
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -16
	movq	%rsi, %rbx
	movq	16(%rdi), %rax
	movsd	(%rax), %xmm0           ## xmm0 = mem[0],zero
	movsd	%xmm0, 8(%rsp)          ## 8-byte Spill
	xorps	%xmm0, %xmm0
	movq	%rsi, %rdi
	callq	_MadList_hasLength
	testb	$1, %al
	je	LBB3_3
## %bb.1:
	xorl	%eax, %eax
	testb	%al, %al
	jne	LBB3_3
## %bb.2:                               ## %branchExpBlock_0
	leaq	_Nothing(%rip), %rax
	jmp	LBB3_9
LBB3_3:                                 ## %nextBlock_0
	movsd	LCPI3_0(%rip), %xmm0    ## xmm0 = mem[0],zero
	movq	%rbx, %rdi
	callq	_MadList_hasMinLength
	movl	%eax, %ecx
	xorl	%eax, %eax
	testb	$1, %cl
	je	LBB3_9
## %bb.4:                               ## %nextBlock_0
	xorl	%ecx, %ecx
	testb	%cl, %cl
	jne	LBB3_9
## %bb.5:                               ## %nextBlock_0
	xorl	%ecx, %ecx
	testb	%cl, %cl
	jne	LBB3_9
## %bb.6:                               ## %branchExpBlock_1
	xorps	%xmm0, %xmm0
	movsd	8(%rsp), %xmm1          ## 8-byte Reload
                                        ## xmm1 = mem[0],zero
	ucomisd	%xmm0, %xmm1
	jne	LBB3_8
	jnp	LBB3_7
LBB3_8:                                 ## %falsyBlock_0
	movq	8(%rbx), %rbx
	addsd	LCPI3_1(%rip), %xmm1
	movsd	%xmm1, 8(%rsp)          ## 8-byte Spill
	movl	$8, %edi
	callq	_GC_malloc
	movsd	8(%rsp), %xmm0          ## 8-byte Reload
                                        ## xmm0 = mem[0],zero
	movsd	%xmm0, (%rax)
	movq	_nth+8(%rip), %rdi
	movq	%rax, %rsi
	callq	*_nth(%rip)
	movq	8(%rax), %rdi
	movq	%rbx, %rsi
	callq	*(%rax)
	jmp	LBB3_9
LBB3_7:                                 ## %truthyBlock_0
	movq	(%rbx), %rsi
	movq	_Just+8(%rip), %rdi
	callq	*_Just(%rip)
LBB3_9:                                 ## %exitBlock_0
	addq	$16, %rsp
	popq	%rbx
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__literal8,8byte_literals
	.p2align	3               ## -- Begin function $closureFn$1
LCPI4_0:
	.quad	4607182418800017408     ## double 1
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_$closureFn$1
	.p2align	4, 0x90
_$closureFn$1:                          ## @"$closureFn$1"
	.cfi_startproc
## %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	movsd	(%rsi), %xmm0           ## xmm0 = mem[0],zero
	addsd	LCPI4_0(%rip), %xmm0
	movsd	%xmm0, (%rsp)           ## 8-byte Spill
	movl	$8, %edi
	callq	_GC_malloc
	movsd	(%rsp), %xmm0           ## 8-byte Reload
                                        ## xmm0 = mem[0],zero
	movsd	%xmm0, (%rax)
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__literal8,8byte_literals
	.p2align	3               ## -- Begin function $closureFn$2
LCPI5_0:
	.quad	4607182418800017408     ## double 1
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_$closureFn$2
	.p2align	4, 0x90
_$closureFn$2:                          ## @"$closureFn$2"
	.cfi_startproc
## %bb.0:
	pushq	%r15
	.cfi_def_cfa_offset 16
	pushq	%r14
	.cfi_def_cfa_offset 24
	pushq	%rbx
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -32
	.cfi_offset %r14, -24
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	(%rdi), %r14
	movsd	LCPI5_0(%rip), %xmm0    ## xmm0 = mem[0],zero
	movq	%rsi, %rdi
	callq	_MadList_hasMinLength
	testb	$1, %al
	je	LBB5_4
## %bb.1:
	xorl	%eax, %eax
	testb	%al, %al
	jne	LBB5_4
## %bb.2:
	xorl	%eax, %eax
	testb	%al, %al
	jne	LBB5_4
## %bb.3:                               ## %branchExpBlock_0
	movq	(%rbx), %r15
	movq	8(%rbx), %rbx
	movq	_$Functor$List(%rip), %rax
	movq	8(%rax), %rdi
	movq	%r14, %rsi
	callq	*(%rax)
	movq	8(%rax), %rdi
	movq	%rbx, %rsi
	callq	*(%rax)
	movq	%rax, %rbx
	movq	8(%r14), %rdi
	movq	%r15, %rsi
	callq	*(%r14)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	_MadList_push
	jmp	LBB5_7
LBB5_4:                                 ## %nextBlock_0
	xorps	%xmm0, %xmm0
	movq	%rbx, %rdi
	callq	_MadList_hasLength
	movl	%eax, %ecx
	xorl	%eax, %eax
	testb	$1, %cl
	je	LBB5_7
## %bb.5:                               ## %nextBlock_0
	xorl	%ecx, %ecx
	testb	%cl, %cl
	jne	LBB5_7
## %bb.6:                               ## %branchExpBlock_1
	xorl	%eax, %eax
LBB5_7:                                 ## %exitBlock_0
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_log$0                  ## -- Begin function log$0
	.p2align	4, 0x90
_log$0:                                 ## @"log$0"
	.cfi_startproc
## %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rsi, %rdi
	callq	_puts
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_showNumber$0           ## -- Begin function showNumber$0
	.p2align	4, 0x90
_showNumber$0:                          ## @"showNumber$0"
	.cfi_startproc
## %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	movsd	(%rsi), %xmm0           ## xmm0 = mem[0],zero
	callq	___doubleToStr__
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_fn$fn                  ## -- Begin function fn$fn
	.p2align	4, 0x90
_fn$fn:                                 ## @"fn$fn"
	.cfi_startproc
## %bb.0:
	movq	_l1(%rip), %rax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_nth$fn                 ## -- Begin function nth$fn
	.p2align	4, 0x90
_nth$fn:                                ## @"nth$fn"
	.cfi_startproc
## %bb.0:
	pushq	%r15
	.cfi_def_cfa_offset 16
	pushq	%r14
	.cfi_def_cfa_offset 24
	pushq	%r12
	.cfi_def_cfa_offset 32
	pushq	%rbx
	.cfi_def_cfa_offset 40
	pushq	%rax
	.cfi_def_cfa_offset 48
	.cfi_offset %rbx, -40
	.cfi_offset %r12, -32
	.cfi_offset %r14, -24
	.cfi_offset %r15, -16
	movsd	(%rsi), %xmm0           ## xmm0 = mem[0],zero
	movsd	%xmm0, (%rsp)           ## 8-byte Spill
	leaq	_Just(%rip), %r15
	leaq	_Nothing(%rip), %r12
	movl	$8, %edi
	callq	_GC_malloc
	movq	%rax, %r14
	movsd	(%rsp), %xmm0           ## 8-byte Reload
                                        ## xmm0 = mem[0],zero
	movsd	%xmm0, (%rax)
	movl	$24, %edi
	callq	_GC_malloc
	movq	%rax, %rbx
	movq	%r15, (%rax)
	movq	%r12, 8(%rax)
	movq	%r14, 16(%rax)
	movl	$16, %edi
	callq	_GC_malloc
	leaq	_$closureFn$0(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rbx, 8(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_getTop$fn              ## -- Begin function getTop$fn
	.p2align	4, 0x90
_getTop$fn:                             ## @"getTop$fn"
	.cfi_startproc
## %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	movsd	_top(%rip), %xmm0       ## xmm0 = mem[0],zero
	movsd	%xmm0, (%rsp)           ## 8-byte Spill
	movl	$8, %edi
	callq	_GC_malloc
	movsd	(%rsp), %xmm0           ## 8-byte Reload
                                        ## xmm0 = mem[0],zero
	movsd	%xmm0, (%rax)
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__literal8,8byte_literals
	.p2align	3               ## -- Begin function hop$fn
LCPI11_0:
	.quad	-4616189618054758400    ## double -1
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_hop$fn
	.p2align	4, 0x90
_hop$fn:                                ## @"hop$fn"
	.cfi_startproc
## %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	movsd	(%rsi), %xmm0           ## xmm0 = mem[0],zero
	movsd	%xmm0, _a(%rip)
	addsd	LCPI11_0(%rip), %xmm0
	movsd	%xmm0, (%rsp)           ## 8-byte Spill
	movl	$8, %edi
	callq	_GC_malloc
	movsd	(%rsp), %xmm0           ## 8-byte Reload
                                        ## xmm0 = mem[0],zero
	movsd	%xmm0, (%rax)
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__literal8,8byte_literals
	.p2align	3               ## -- Begin function createBigList$fn
LCPI12_0:
	.quad	-4616189618054758400    ## double -1
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_createBigList$fn
	.p2align	4, 0x90
_createBigList$fn:                      ## @"createBigList$fn"
	.cfi_startproc
## %bb.0:
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -16
	movsd	(%rsi), %xmm1           ## xmm1 = mem[0],zero
	xorps	%xmm0, %xmm0
	ucomisd	%xmm0, %xmm1
	jne	LBB12_2
	jnp	LBB12_1
LBB12_2:                                ## %falsyBlock_0
	addsd	LCPI12_0(%rip), %xmm1
	movsd	%xmm1, 8(%rsp)          ## 8-byte Spill
	movl	$8, %edi
	callq	_GC_malloc
	movsd	8(%rsp), %xmm0          ## 8-byte Reload
                                        ## xmm0 = mem[0],zero
	movsd	%xmm0, (%rax)
	movq	_createBigList+8(%rip), %rdi
	movq	%rax, %rsi
	callq	*_createBigList(%rip)
	movq	%rax, %rbx
	movl	$8, %edi
	callq	_GC_malloc
	movabsq	$4621256167635550208, %rcx ## imm = 0x4022000000000000
	movq	%rcx, (%rax)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	_MadList_push
	jmp	LBB12_3
LBB12_1:                                ## %truthyBlock_0
	xorl	%eax, %eax
LBB12_3:                                ## %condBlock_0
	addq	$16, %rsp
	popq	%rbx
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__literal8,8byte_literals
	.p2align	3               ## -- Begin function main
LCPI13_0:
	.quad	4607182418800017408     ## double 1
LCPI13_1:
	.quad	4613937818241073152     ## double 3
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry_0
	pushq	%rbp
	.cfi_def_cfa_offset 16
	pushq	%r15
	.cfi_def_cfa_offset 24
	pushq	%r14
	.cfi_def_cfa_offset 32
	pushq	%r13
	.cfi_def_cfa_offset 40
	pushq	%r12
	.cfi_def_cfa_offset 48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	pushq	%rax
	.cfi_def_cfa_offset 64
	.cfi_offset %rbx, -56
	.cfi_offset %r12, -48
	.cfi_offset %r13, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	.cfi_offset %rbp, -16
	movl	$8, %edi
	callq	_GC_malloc
	movabsq	$4613937818241073152, %r12 ## imm = 0x4008000000000000
	movq	%r12, (%rax)
	movq	%rax, %rdi
	callq	_MadList_singleton
	movq	%rax, %rbx
	movl	$8, %edi
	callq	_GC_malloc
	movabsq	$4611686018427387904, %r15 ## imm = 0x4000000000000000
	movq	%r15, (%rax)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	_MadList_push
	movq	%rax, %rbx
	movl	$8, %edi
	callq	_GC_malloc
	movabsq	$4607182418800017408, %rcx ## imm = 0x3FF0000000000000
	movq	%rcx, (%rax)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	_MadList_push
	movq	%rax, %r14
	movq	%rax, _l1(%rip)
	movl	$8, %edi
	callq	_GC_malloc
	movabsq	$4618441417868443648, %rcx ## imm = 0x4018000000000000
	movq	%rcx, (%rax)
	movq	%rax, %rdi
	callq	_MadList_singleton
	movq	%rax, %rbx
	movl	$8, %edi
	callq	_GC_malloc
	movabsq	$4617315517961601024, %rbp ## imm = 0x4014000000000000
	movq	%rbp, (%rax)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	_MadList_push
	movq	%rax, %rbx
	movl	$8, %edi
	callq	_GC_malloc
	movabsq	$4616189618054758400, %r13 ## imm = 0x4010000000000000
	movq	%r13, (%rax)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	_MadList_push
	movq	%rax, _l2(%rip)
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	_MadList_concat
	movq	%rax, %r14
	movq	%rax, _l3(%rip)
	movl	$8, %edi
	callq	_GC_malloc
	movq	%r15, (%rax)
	movq	_fn+8(%rip), %rdi
	movq	%rax, %rsi
	callq	*_fn(%rip)
	movq	%rax, %rbx
	movsd	LCPI13_0(%rip), %xmm0   ## xmm0 = mem[0],zero
	movq	%rax, %rdi
	callq	_MadList_hasMinLength
	xorl	%esi, %esi
	testb	$1, %al
	je	LBB13_4
## %bb.1:                               ## %entry_0
	xorl	%eax, %eax
	testb	%al, %al
	jne	LBB13_4
## %bb.2:                               ## %entry_0
	xorl	%eax, %eax
	testb	%al, %al
	jne	LBB13_4
## %bb.3:                               ## %branchExpBlock_0
	movq	(%rbx), %rbx
	callq	*_$Show$Number(%rip)
	movq	8(%rax), %rdi
	movq	%rbx, %rsi
	callq	*(%rax)
	movq	%rax, %rsi
LBB13_4:                                ## %exitBlock_0
	movq	_log+8(%rip), %rdi
	callq	*_log(%rip)
	movq	_$Functor$List(%rip), %rbx
	callq	*_$Show$Number(%rip)
	movq	8(%rbx), %rdi
	movq	%rax, %rsi
	callq	*(%rbx)
	movq	8(%rax), %rdi
	movq	%r14, %rsi
	callq	*(%rax)
	movq	%rax, %rbx
	movsd	LCPI13_1(%rip), %xmm0   ## xmm0 = mem[0],zero
	movq	%rax, %rdi
	callq	_MadList_hasMinLength
	xorl	%esi, %esi
	testb	$1, %al
	je	LBB13_10
## %bb.5:                               ## %exitBlock_0
	xorl	%eax, %eax
	testb	%al, %al
	jne	LBB13_10
## %bb.6:                               ## %exitBlock_0
	xorl	%eax, %eax
	testb	%al, %al
	jne	LBB13_10
## %bb.7:                               ## %exitBlock_0
	xorl	%eax, %eax
	testb	%al, %al
	jne	LBB13_10
## %bb.8:                               ## %exitBlock_0
	xorl	%eax, %eax
	testb	%al, %al
	jne	LBB13_10
## %bb.9:                               ## %branchExpBlock_1
	movq	8(%rbx), %rax
	movq	8(%rax), %rax
	movq	(%rax), %rsi
LBB13_10:                               ## %exitBlock_1
	movq	%rsi, _mapped(%rip)
	movq	_log+8(%rip), %rdi
	callq	*_log(%rip)
	movl	$8, %edi
	callq	_GC_malloc
	movq	%r12, (%rax)
	movq	_nth+8(%rip), %rdi
	movq	%rax, %rsi
	callq	*_nth(%rip)
	movq	%rax, %r14
	movl	$8, %edi
	callq	_GC_malloc
	movq	%rbp, (%rax)
	movq	%rax, %rdi
	callq	_MadList_singleton
	movq	%rax, %rbx
	movl	$8, %edi
	callq	_GC_malloc
	movq	%r13, (%rax)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	_MadList_push
	movq	%rax, %rbx
	movl	$8, %edi
	callq	_GC_malloc
	movq	%r12, (%rax)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	_MadList_push
	movq	%rax, %rbx
	movl	$8, %edi
	callq	_GC_malloc
	movq	%r15, (%rax)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	_MadList_push
	movq	%rax, %rbx
	movl	$8, %edi
	callq	_GC_malloc
	movabsq	$4607182418800017408, %rcx ## imm = 0x3FF0000000000000
	movq	%rcx, (%rax)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	_MadList_push
	movq	8(%r14), %rdi
	movq	%rax, %rsi
	callq	*(%r14)
	cmpq	$0, (%rax)
	jne	LBB13_14
## %bb.11:                              ## %exitBlock_1
	xorl	%ecx, %ecx
	testb	%cl, %cl
	jne	LBB13_14
## %bb.12:                              ## %exitBlock_1
	xorl	%ecx, %ecx
	testb	%cl, %cl
	jne	LBB13_14
## %bb.13:                              ## %branchExpBlock_2
	movq	8(%rax), %rbx
	callq	*_$Show$Number(%rip)
	movq	8(%rax), %rdi
	movq	%rbx, %rsi
	callq	*(%rax)
	jmp	LBB13_17
LBB13_14:                               ## %nextBlock_0
	cmpq	$1, (%rax)
	jne	LBB13_18
## %bb.15:                              ## %nextBlock_0
	xorl	%eax, %eax
	testb	%al, %al
	jne	LBB13_18
## %bb.16:                              ## %branchExpBlock_3
	movl	$8, %edi
	callq	_GC_malloc
	movabsq	$29113321604280174, %rcx ## imm = 0x676E6968746F6E
	movq	%rcx, (%rax)
LBB13_17:                               ## %exitBlock_2
	movq	_log+8(%rip), %rdi
	movq	%rax, %rsi
	callq	*_log(%rip)
LBB13_18:                               ## %exitBlock_2
	movabsq	$4626041242239631360, %rax ## imm = 0x4033000000000000
	movq	%rax, _top(%rip)
	callq	*_$Show$Number(%rip)
	movq	%rax, %rbx
	movl	$8, %edi
	callq	_GC_malloc
	movq	%r12, (%rax)
	movq	_getTop+8(%rip), %rdi
	movq	%rax, %rsi
	callq	*_getTop(%rip)
	movsd	(%rax), %xmm0           ## xmm0 = mem[0],zero
	movsd	%xmm0, (%rsp)           ## 8-byte Spill
	movl	$8, %edi
	callq	_GC_malloc
	movsd	(%rsp), %xmm0           ## 8-byte Reload
                                        ## xmm0 = mem[0],zero
	movsd	%xmm0, (%rax)
	movq	8(%rbx), %rdi
	movq	%rax, %rsi
	callq	*(%rbx)
	movq	_log+8(%rip), %rdi
	movq	%rax, %rsi
	callq	*_log(%rip)
	movq	%r15, _i(%rip)
	movq	%r13, _j(%rip)
	movabsq	$4621256167635550208, %rax ## imm = 0x4022000000000000
	movq	%rax, _d(%rip)
	movabsq	$4630544841867001856, %rbp ## imm = 0x4043000000000000
	movq	%rbp, _r(%rip)
	callq	*_$Show$Number(%rip)
	movq	%rax, %rbx
	movl	$8, %edi
	callq	_GC_malloc
	movq	%rbp, (%rax)
	movq	8(%rbx), %rdi
	movq	%rax, %rsi
	callq	*(%rbx)
	movq	%rax, _e(%rip)
	movq	_log+8(%rip), %rdi
	movq	%rax, %rsi
	callq	*_log(%rip)
	movl	$8, %edi
	callq	_GC_malloc
	movabsq	$4681608360884174848, %rcx ## imm = 0x40F86A0000000000
	movq	%rcx, (%rax)
	movq	_createBigList+8(%rip), %rdi
	movq	%rax, %rsi
	callq	*_createBigList(%rip)
	movq	%rax, %r14
	movq	%rax, _big(%rip)
	movl	$8, %edi
	callq	_GC_malloc
	movabsq	$28259048195715683, %rcx ## imm = 0x64657461657263
	movq	%rcx, (%rax)
	movq	_log+8(%rip), %rdi
	movq	%rax, %rsi
	callq	*_log(%rip)
	movq	_$Functor$List(%rip), %rbp
	xorl	%edi, %edi
	callq	_GC_malloc
	movq	%rax, %rbx
	movl	$16, %edi
	callq	_GC_malloc
	leaq	_$closureFn$1(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rbx, 8(%rax)
	movq	8(%rbp), %rdi
	movq	%rax, %rsi
	callq	*(%rbp)
	movq	8(%rax), %rdi
	movq	%r14, %rsi
	callq	*(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__DATA,__data
	.globl	_$Show$Number           ## @"$Show$Number"
	.p2align	3
_$Show$Number:
	.quad	_$Show$Number$show

	.globl	_$Functor$List$map      ## @"$Functor$List$map"
	.p2align	3
_$Functor$List$map:
	.quad	_$Functor$List$map$fn
	.quad	_$EMPTY_ENV

	.globl	_$Functor$List          ## @"$Functor$List"
	.p2align	3
_$Functor$List:
	.quad	_$Functor$List$map

	.globl	_Just                   ## @Just
	.p2align	3
_Just:
	.quad	_Just$0
	.quad	0

	.globl	_Nothing                ## @Nothing
	.p2align	3
_Nothing:
	.quad	1                       ## 0x1

	.globl	_log                    ## @log
	.p2align	3
_log:
	.quad	_log$0
	.quad	0

	.globl	_showNumber             ## @showNumber
	.p2align	3
_showNumber:
	.quad	_showNumber$0
	.quad	0

	.globl	_fn                     ## @fn
	.p2align	3
_fn:
	.quad	_fn$fn
	.quad	_$EMPTY_ENV

	.globl	_nth                    ## @nth
	.p2align	3
_nth:
	.quad	_nth$fn
	.quad	_$EMPTY_ENV

	.globl	_getTop                 ## @getTop
	.p2align	3
_getTop:
	.quad	_getTop$fn
	.quad	_$EMPTY_ENV

	.globl	_hop                    ## @hop
	.p2align	3
_hop:
	.quad	_hop$fn
	.quad	_$EMPTY_ENV

	.globl	_a                      ## @a
.zerofill __DATA,__common,_a,8,3
	.globl	_createBigList          ## @createBigList
	.p2align	3
_createBigList:
	.quad	_createBigList$fn
	.quad	_$EMPTY_ENV

	.globl	_$EMPTY_ENV             ## @"$EMPTY_ENV"
.zerofill __DATA,__common,_$EMPTY_ENV,1,3
	.globl	_l1                     ## @l1
.zerofill __DATA,__common,_l1,8,3
	.globl	_l2                     ## @l2
.zerofill __DATA,__common,_l2,8,3
	.globl	_l3                     ## @l3
.zerofill __DATA,__common,_l3,8,3
	.globl	_mapped                 ## @mapped
.zerofill __DATA,__common,_mapped,8,3
	.globl	_top                    ## @top
.zerofill __DATA,__common,_top,8,3
	.globl	_i                      ## @i
.zerofill __DATA,__common,_i,8,3
	.globl	_j                      ## @j
.zerofill __DATA,__common,_j,8,3
	.globl	_d                      ## @d
.zerofill __DATA,__common,_d,8,3
	.globl	_r                      ## @r
.zerofill __DATA,__common,_r,8,3
	.globl	_e                      ## @e
.zerofill __DATA,__common,_e,8,3
	.globl	_big                    ## @big
.zerofill __DATA,__common,_big,8,3

.subsections_via_symbols
