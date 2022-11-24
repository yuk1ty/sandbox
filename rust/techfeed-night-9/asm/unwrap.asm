std::sys_common::backtrace::__rust_begin_short_backtrace:
	pushq	%rax
	callq	*%rdi
	#APP
	#NO_APP
	popq	%rax
	retq

std::rt::lang_start:
	pushq	%rax
	movl	%ecx, %r8d
	movq	%rdx, %rcx
	movq	%rsi, %rdx
	movq	%rdi, (%rsp)
	leaq	.L__unnamed_1(%rip), %rsi
	movq	%rsp, %rdi
	callq	*std::rt::lang_start_internal@GOTPCREL(%rip)
	popq	%rcx
	retq

std::rt::lang_start::{{closure}}:
	pushq	%rax
	movq	(%rdi), %rdi
	callq	std::sys_common::backtrace::__rust_begin_short_backtrace
	xorl	%eax, %eax
	popq	%rcx
	retq

core::ops::function::FnOnce::call_once{{vtable.shim}}:
	pushq	%rax
	movq	(%rdi), %rdi
	callq	std::sys_common::backtrace::__rust_begin_short_backtrace
	xorl	%eax, %eax
	popq	%rcx
	retq

core::ptr::drop_in_place<std::env::Args>:
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	movq	%rdi, %r14
	movq	16(%rdi), %r12
	movq	24(%rdi), %rcx
	movq	%rcx, %rax
	subq	%r12, %rax
	movabsq	$-6148914691236517205, %rdx
	mulq	%rdx
	cmpq	%r12, %rcx
	je	.LBB4_5
	shrq	$4, %rdx
	shlq	$3, %rdx
	leaq	(%rdx,%rdx,2), %r15
	xorl	%ebx, %ebx
	movq	__rust_dealloc@GOTPCREL(%rip), %r13
	jmp	.LBB4_2

.LBB4_4:
	addq	$24, %rbx
	cmpq	%rbx, %r15
	je	.LBB4_5

.LBB4_2:
	movq	8(%r12,%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB4_4
	movq	(%r12,%rbx), %rdi
	movq	%rsi, %rdx
	notq	%rdx
	shrq	$63, %rdx
	callq	*%r13
	jmp	.LBB4_4

.LBB4_5:
	movq	8(%r14), %rax
	testq	%rax, %rax
	je	.LBB4_6
	movq	(%r14), %rdi
	shlq	$3, %rax
	leaq	(%rax,%rax,2), %rsi
	movl	$8, %edx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmpq	*__rust_dealloc@GOTPCREL(%rip)

.LBB4_6:
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq

core::ptr::drop_in_place<alloc::string::String>:
	movq	8(%rdi), %rsi
	testq	%rsi, %rsi
	je	.LBB5_1
	movq	(%rdi), %rdi
	movq	%rsi, %rdx
	notq	%rdx
	shrq	$63, %rdx
	jmpq	*__rust_dealloc@GOTPCREL(%rip)

.LBB5_1:
	retq

core::ptr::drop_in_place<core::num::error::ParseIntError>:
	retq

core::ptr::drop_in_place<alloc::vec::Vec<alloc::string::String>>:
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	movq	%rdi, %r14
	movq	16(%rdi), %rax
	testq	%rax, %rax
	je	.LBB7_5
	movq	(%r14), %r12
	shlq	$3, %rax
	leaq	(%rax,%rax,2), %r15
	xorl	%ebx, %ebx
	movq	__rust_dealloc@GOTPCREL(%rip), %r13
	jmp	.LBB7_2

.LBB7_4:
	addq	$24, %rbx
	cmpq	%rbx, %r15
	je	.LBB7_5

.LBB7_2:
	movq	8(%r12,%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB7_4
	movq	(%r12,%rbx), %rdi
	movq	%rsi, %rdx
	notq	%rdx
	shrq	$63, %rdx
	callq	*%r13
	jmp	.LBB7_4

.LBB7_5:
	movq	8(%r14), %rax
	testq	%rax, %rax
	je	.LBB7_6
	movq	(%r14), %rdi
	shlq	$3, %rax
	leaq	(%rax,%rax,2), %rsi
	movl	$8, %edx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmpq	*__rust_dealloc@GOTPCREL(%rip)

.LBB7_6:
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq

alloc::raw_vec::finish_grow:
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	movq	%rsi, %r15
	movq	%rdi, %rbx
	testq	%rdx, %rdx
	je	.LBB8_5
	movq	%rdx, %r14
	cmpq	$0, 16(%rcx)
	je	.LBB8_3
	movq	8(%rcx), %rsi
	testq	%rsi, %rsi
	je	.LBB8_3
	movq	(%rcx), %rdi
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	*__rust_realloc@GOTPCREL(%rip)
	movq	%r15, %rcx
	testq	%rax, %rax
	jne	.LBB8_12
	jmp	.LBB8_13

.LBB8_3:
	testq	%r15, %r15
	je	.LBB8_4
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	*__rust_alloc@GOTPCREL(%rip)
	movq	%r15, %rcx
	testq	%rax, %rax
	je	.LBB8_13

.LBB8_12:
	movq	%rax, 8(%rbx)
	movq	%rcx, 16(%rbx)
	xorl	%eax, %eax
	jmp	.LBB8_7

.LBB8_5:
	movq	%r15, 8(%rbx)
	movq	$0, 16(%rbx)
	jmp	.LBB8_6

.LBB8_4:
	xorl	%ecx, %ecx
	movq	%r14, %rax
	testq	%rax, %rax
	jne	.LBB8_12

.LBB8_13:
	movq	%r15, 8(%rbx)
	movq	%r14, 16(%rbx)

.LBB8_6:
	movl	$1, %eax

.LBB8_7:
	movq	%rax, (%rbx)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq

alloc::raw_vec::RawVec<T,A>::reserve::do_reserve_and_handle:
	pushq	%r14
	pushq	%rbx
	subq	$56, %rsp
	addq	%rdx, %rsi
	jb	.LBB9_10
	movq	%rdi, %r14
	movq	8(%rdi), %rax
	leaq	(%rax,%rax), %rcx
	cmpq	%rsi, %rcx
	cmovaq	%rcx, %rsi
	cmpq	$5, %rsi
	movl	$4, %ebx
	cmovaeq	%rsi, %rbx
	movabsq	$384307168202282325, %rcx
	xorl	%edx, %edx
	cmpq	%rcx, %rbx
	setbe	%dl
	leaq	(,%rbx,8), %rcx
	leaq	(%rcx,%rcx,2), %rsi
	shlq	$3, %rdx
	testq	%rax, %rax
	je	.LBB9_3
	movq	(%r14), %rcx
	shlq	$3, %rax
	leaq	(%rax,%rax,2), %rax
	movq	%rcx, 8(%rsp)
	movq	%rax, 16(%rsp)
	movq	$8, 24(%rsp)
	jmp	.LBB9_4

.LBB9_3:
	movq	$0, 24(%rsp)

.LBB9_4:
	leaq	32(%rsp), %rdi
	leaq	8(%rsp), %rcx
	callq	alloc::raw_vec::finish_grow
	cmpq	$0, 32(%rsp)
	movq	40(%rsp), %rdi
	je	.LBB9_5
	movq	48(%rsp), %rsi
	movabsq	$-9223372036854775807, %rax
	cmpq	%rax, %rsi
	jne	.LBB9_8
	addq	$56, %rsp
	popq	%rbx
	popq	%r14
	retq

.LBB9_5:
	movq	%rdi, (%r14)
	movq	%rbx, 8(%r14)
	addq	$56, %rsp
	popq	%rbx
	popq	%r14
	retq

.LBB9_8:
	testq	%rsi, %rsi
	jne	.LBB9_9

.LBB9_10:
	callq	*alloc::raw_vec::capacity_overflow@GOTPCREL(%rip)
	ud2

.LBB9_9:
	callq	*alloc::alloc::handle_alloc_error@GOTPCREL(%rip)
	ud2

playground::main:
	pushq	%rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$232, %rsp
	leaq	200(%rsp), %rdi
	callq	*std::env::args@GOTPCREL(%rip)
	movups	200(%rsp), %xmm0
	movups	216(%rsp), %xmm1
	movaps	%xmm1, 48(%rsp)
	movaps	%xmm0, 32(%rsp)
	leaq	144(%rsp), %rdi
	leaq	32(%rsp), %rsi
	callq	*<std::env::Args as core::iter::traits::iterator::Iterator>::next@GOTPCREL(%rip)
	cmpq	$0, 144(%rsp)
	je	.LBB10_2
	movq	160(%rsp), %rax
	movq	%rax, 16(%rsp)
	movups	144(%rsp), %xmm0
	movaps	%xmm0, (%rsp)
	leaq	96(%rsp), %rdi
	leaq	32(%rsp), %rsi
	callq	*<std::env::Args as core::iter::traits::iterator::Iterator>::size_hint@GOTPCREL(%rip)
	movq	96(%rsp), %rax
	incq	%rax
	movq	$-1, %rcx
	cmovneq	%rax, %rcx
	cmpq	$5, %rcx
	movl	$4, %r15d
	cmovaeq	%rcx, %r15
	movabsq	$384307168202282326, %rax
	decq	%rax
	xorl	%ebp, %ebp
	cmpq	%rax, %r15
	setbe	%al
	ja	.LBB10_14
	leaq	(,%r15,8), %rcx
	movb	%al, %bpl
	shlq	$3, %rbp
	leaq	(%rcx,%rcx,2), %r14
	testq	%r14, %r14
	je	.LBB10_17
	movq	%r14, %rdi
	movq	%rbp, %rsi
	callq	*__rust_alloc@GOTPCREL(%rip)
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB10_20

.LBB10_21:
	movq	160(%rsp), %rax
	movq	%rax, 16(%rbx)
	movups	144(%rsp), %xmm0
	movups	%xmm0, (%rbx)
	movq	%rbx, 72(%rsp)
	movq	%r15, 80(%rsp)
	movq	$1, 88(%rsp)
	movaps	32(%rsp), %xmm0
	movaps	48(%rsp), %xmm1
	movaps	%xmm1, 112(%rsp)
	movaps	%xmm0, 96(%rsp)
	movl	$2, %ebp
	movl	$24, %r14d
	leaq	96(%rsp), %r13
	movq	<std::env::Args as core::iter::traits::iterator::Iterator>::next@GOTPCREL(%rip), %r12
	jmp	.LBB10_22

.LBB10_28:
	movq	192(%rsp), %rax
	movq	%rax, 16(%rbx,%r14)
	movups	176(%rsp), %xmm0
	movups	%xmm0, (%rbx,%r14)
	movq	%rbp, 88(%rsp)
	incq	%rbp
	addq	$24, %r14

.LBB10_22:
	leaq	176(%rsp), %rdi
	movq	%r13, %rsi
	callq	*%r12
	cmpq	$0, 176(%rsp)
	je	.LBB10_32
	leaq	-1(%rbp), %r15
	movq	192(%rsp), %rax
	movq	%rax, 160(%rsp)
	movups	176(%rsp), %xmm0
	movaps	%xmm0, 144(%rsp)
	cmpq	80(%rsp), %r15
	jne	.LBB10_28
	movq	%rsp, %rdi
	movq	%r13, %rsi
	callq	*<std::env::Args as core::iter::traits::iterator::Iterator>::size_hint@GOTPCREL(%rip)
	movq	(%rsp), %rdx
	incq	%rdx
	movq	$-1, %rax
	cmoveq	%rax, %rdx
	leaq	72(%rsp), %rdi
	movq	%r15, %rsi
	callq	alloc::raw_vec::RawVec<T,A>::reserve::do_reserve_and_handle
	movq	72(%rsp), %rbx
	jmp	.LBB10_28

.LBB10_32:
	movq	112(%rsp), %rbx
	movq	120(%rsp), %rcx
	movq	%rcx, %rax
	subq	%rbx, %rax
	movabsq	$-6148914691236517205, %rdx
	mulq	%rdx
	cmpq	%rbx, %rcx
	je	.LBB10_37
	shrq	$4, %rdx
	shlq	$3, %rdx
	leaq	(%rdx,%rdx,2), %r14
	xorl	%ebp, %ebp
	movq	__rust_dealloc@GOTPCREL(%rip), %r15
	jmp	.LBB10_34

.LBB10_36:
	addq	$24, %rbp
	cmpq	%rbp, %r14
	je	.LBB10_37

.LBB10_34:
	movq	8(%rbx,%rbp), %rsi
	testq	%rsi, %rsi
	je	.LBB10_36
	movq	(%rbx,%rbp), %rdi
	movq	%rsi, %rdx
	notq	%rdx
	shrq	$63, %rdx
	callq	*%r15
	jmp	.LBB10_36

.LBB10_37:
	movq	104(%rsp), %rax
	testq	%rax, %rax
	je	.LBB10_39
	movq	96(%rsp), %rdi
	shlq	$3, %rax
	leaq	(%rax,%rax,2), %rsi
	movl	$8, %edx
	callq	*__rust_dealloc@GOTPCREL(%rip)

.LBB10_39:
	movups	72(%rsp), %xmm0
	movaps	%xmm0, (%rsp)
	movq	88(%rsp), %rbp
	movq	%rbp, 16(%rsp)
	cmpq	$1, %rbp
	jbe	.LBB10_10
	movq	(%rsp), %r14
	movq	24(%r14), %rdi
	movq	40(%r14), %rsi
	callq	*core::num::<impl core::str::traits::FromStr for i32>::from_str@GOTPCREL(%rip)
	testb	$1, %al
	jne	.LBB10_42
	shrq	$32, %rax
	movl	%eax, 200(%rsp)
	leaq	200(%rsp), %rax
	movq	%rax, 32(%rsp)
	movq	core::fmt::num::imp::<impl core::fmt::Display for i32>::fmt@GOTPCREL(%rip), %rax
	movq	%rax, 40(%rsp)
	leaq	.L__unnamed_2(%rip), %rax
	movq	%rax, 96(%rsp)
	movq	$2, 104(%rsp)
	movq	$0, 112(%rsp)
	leaq	32(%rsp), %rax
	movq	%rax, 128(%rsp)
	movq	$1, 136(%rsp)
	leaq	96(%rsp), %rdi
	callq	*std::io::stdio::_print@GOTPCREL(%rip)
	shlq	$3, %rbp
	leaq	(%rbp,%rbp,2), %rbp
	xorl	%ebx, %ebx
	movq	__rust_dealloc@GOTPCREL(%rip), %r15
	jmp	.LBB10_50

.LBB10_52:
	addq	$24, %rbx
	cmpq	%rbx, %rbp
	je	.LBB10_53

.LBB10_50:
	movq	8(%r14,%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB10_52
	movq	(%r14,%rbx), %rdi
	movq	%rsi, %rdx
	notq	%rdx
	shrq	$63, %rdx
	callq	*%r15
	jmp	.LBB10_52

.LBB10_53:
	movq	8(%rsp), %rax
	testq	%rax, %rax
	je	.LBB10_55
	shlq	$3, %rax
	leaq	(%rax,%rax,2), %rsi
	movl	$8, %edx
	movq	%r14, %rdi
	callq	*__rust_dealloc@GOTPCREL(%rip)

.LBB10_55:
	addq	$232, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq

.LBB10_17:
	movq	%rbp, %rbx
	testq	%rbx, %rbx
	jne	.LBB10_21

.LBB10_20:
	movq	%r14, %rdi
	movq	%rbp, %rsi
	callq	*alloc::alloc::handle_alloc_error@GOTPCREL(%rip)
	jmp	.LBB10_15

.LBB10_2:
	movq	48(%rsp), %rbx
	movq	56(%rsp), %rcx
	movq	%rcx, %rax
	subq	%rbx, %rax
	movabsq	$-6148914691236517205, %rdx
	mulq	%rdx
	movq	$8, (%rsp)
	xorps	%xmm0, %xmm0
	movups	%xmm0, 8(%rsp)
	cmpq	%rbx, %rcx
	jne	.LBB10_3

.LBB10_7:
	movq	40(%rsp), %rax
	testq	%rax, %rax
	je	.LBB10_9
	movq	32(%rsp), %rdi
	shlq	$3, %rax
	leaq	(%rax,%rax,2), %rsi
	movl	$8, %edx
	callq	*__rust_dealloc@GOTPCREL(%rip)

.LBB10_9:
	xorl	%ebp, %ebp

.LBB10_10:
	leaq	.L__unnamed_3(%rip), %rdx
	movl	$1, %edi
	movq	%rbp, %rsi
	callq	*core::panicking::panic_bounds_check@GOTPCREL(%rip)
	jmp	.LBB10_15

.LBB10_14:
	callq	*alloc::raw_vec::capacity_overflow@GOTPCREL(%rip)
	jmp	.LBB10_15

.LBB10_42:
	movb	%ah, 96(%rsp)
	leaq	.L__unnamed_4(%rip), %rdi
	leaq	.L__unnamed_5(%rip), %rcx
	leaq	.L__unnamed_6(%rip), %r8
	leaq	96(%rsp), %rdx
	movl	$43, %esi
	callq	*core::result::unwrap_failed@GOTPCREL(%rip)

.LBB10_15:
	ud2

.LBB10_3:
	shrq	$4, %rdx
	shlq	$3, %rdx
	leaq	(%rdx,%rdx,2), %r14
	xorl	%ebp, %ebp
	movq	__rust_dealloc@GOTPCREL(%rip), %r15
	jmp	.LBB10_4

.LBB10_6:
	addq	$24, %rbp
	cmpq	%rbp, %r14
	je	.LBB10_7

.LBB10_4:
	movq	8(%rbx,%rbp), %rsi
	testq	%rsi, %rsi
	je	.LBB10_6
	movq	(%rbx,%rbp), %rdi
	movq	%rsi, %rdx
	notq	%rdx
	shrq	$63, %rdx
	callq	*%r15
	jmp	.LBB10_6
	movq	%rax, %rbx
	jmp	.LBB10_46
	movq	%rax, %rbx
	movq	%rsp, %rdi
	callq	core::ptr::drop_in_place<alloc::string::String>

.LBB10_46:
	leaq	32(%rsp), %rdi
	callq	core::ptr::drop_in_place<std::env::Args>
	jmp	.LBB10_31
	movq	%rax, %rbx
	leaq	144(%rsp), %rdi
	callq	core::ptr::drop_in_place<alloc::string::String>
	jmp	.LBB10_30
	movq	%rax, %rbx

.LBB10_30:
	leaq	96(%rsp), %rdi
	callq	core::ptr::drop_in_place<std::env::Args>
	leaq	72(%rsp), %rdi
	callq	core::ptr::drop_in_place<alloc::vec::Vec<alloc::string::String>>
	jmp	.LBB10_31
	callq	*core::panicking::panic_no_unwind@GOTPCREL(%rip)
	ud2
	movq	%rax, %rbx
	movq	%rsp, %rdi
	callq	core::ptr::drop_in_place<alloc::vec::Vec<alloc::string::String>>

.LBB10_31:
	movq	%rbx, %rdi
	callq	_Unwind_Resume@PLT
	ud2
	callq	*core::panicking::panic_no_unwind@GOTPCREL(%rip)
	ud2

main:
	pushq	%rax
	movq	%rsi, %rcx
	movslq	%edi, %rdx
	leaq	playground::main(%rip), %rax
	movq	%rax, (%rsp)
	leaq	.L__unnamed_1(%rip), %rsi
	movq	%rsp, %rdi
	movl	$2, %r8d
	callq	*std::rt::lang_start_internal@GOTPCREL(%rip)
	popq	%rcx
	retq

.L__unnamed_1:
	.quad	core::ptr::drop_in_place<core::num::error::ParseIntError>
	.asciz	"\b\000\000\000\000\000\000\000\b\000\000\000\000\000\000"
	.quad	core::ops::function::FnOnce::call_once{{vtable.shim}}
	.quad	std::rt::lang_start::{{closure}}
	.quad	std::rt::lang_start::{{closure}}

.L__unnamed_7:

.L__unnamed_4:
	.ascii	"called `Result::unwrap()` on an `Err` value"

.L__unnamed_5:
	.quad	core::ptr::drop_in_place<core::num::error::ParseIntError>
	.asciz	"\001\000\000\000\000\000\000\000\001\000\000\000\000\000\000"
	.quad	<core::num::error::ParseIntError as core::fmt::Debug>::fmt

.L__unnamed_8:
	.ascii	"src/main.rs"

.L__unnamed_3:
	.quad	.L__unnamed_8
	.asciz	"\013\000\000\000\000\000\000\000\003\000\000\000\024\000\000"

.L__unnamed_6:
	.quad	.L__unnamed_8
	.asciz	"\013\000\000\000\000\000\000\000\003\000\000\000$\000\000"

.L__unnamed_9:
	.byte	10

.L__unnamed_2:
	.quad	.L__unnamed_7
	.zero	8
	.quad	.L__unnamed_9
	.asciz	"\001\000\000\000\000\000\000"
