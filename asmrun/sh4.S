/* SH4 runtime support routines */

#define trap_ptr r14
#define alloc_ptr r12
#define alloc_limit r13

	.text
	.global caml_call_gc
caml_call_gc:
	/* FIXME: Not implemented. */
	rts
	nop	! delay slot

	/* Call a C function from OCaml. Function to call is in r11.
	   The callee preserves r8-r15. r8-r11 are good to use as
	   temporaries. Modelled on ARM implementation. */

	.global caml_c_call
caml_c_call:
	/* Return address in callee-save register. */
	sts	pr, r8

	/* Record lowest stack address and return address */
	mov.l	.Lcaml_bottom_of_stack, r9
	mov.l	.Lcaml_last_return_address, r10
	add	#4, r10
	sts.l	pr, @-r10
	mov.l	r15, @r9

	/* Make the exception handler and alloc ptr available to the C code */
	mov.l	.Lcaml_exception_pointer, r9
	mov.l	trap_ptr, @r9
	mov.l	.Lcaml_young_ptr, r9

	/* Call the function */
	jsr	@r11
	mov.l	alloc_ptr, @r9	! delay slot

	/* Reload alloc ptr, r9 still points to caml_young_ptr */
	mov.l	@r9, alloc_ptr

	/* Say that we are back into OCaml code */
	mov	#0, r9

	/* Return. */
	jmp	@r8
	/* r10 is still caml_last_return_address */
	mov.l	r9, @r10	! delay slot

	/* Amount to allocate is in r10. Can use r10, r11 as temporaries (and
	   r0-r9 if preserved on the stack). Memory is allocated at decreasing
	   addresses from alloc_ptr, until *alloc_limit is reached. Then, the
	   garbage collector must be invoked. Modelled on ARM implementation. */

	.global caml_allocN
caml_allocN:
	mov.l	@alloc_limit, r11
	sub	r10, alloc_ptr
	cmp/hs	r11, alloc_ptr
	bf	beyond_alloc_limit
	rts				/* Return if alloc_ptr >= alloc_limit */
	nop	! delay slot
beyond_alloc_limit:
	/* Record return address and desired size (alloc_limit register used as
	   temp) */
	mov.l	.Lcaml_last_return_address, alloc_limit
	add	#4, alloc_limit
	sts.l	pr, @-alloc_limit
	mov.l	.Lcaml_requested_size, alloc_limit
	/* Invoke GC */
	bsr	invoke_gc
	mov.l	r10, @alloc_limit	! delay slot
	/* Try again */
	mov.l	.Lcaml_requested_size, r10
	mov.l	@r10, r10
	bra	caml_allocN

invoke_gc:
	/* Record lowest stack address */
	mov.l	.Lcaml_bottom_of_stack, r10
	mov.l	r15, @r10
	/* Save integer registers and return address on stack */
	sts.l	pr, @-r15
	mov.l	r9, @-r15
	mov.l	r8, @-r15
	mov.l	r7, @-r15
	mov.l	r6, @-r15
	mov.l	r5, @-r15
	mov.l	r4, @-r15
	mov.l	r3, @-r15
	mov.l	r2, @-r15
	mov.l	r1, @-r15
	mov.l	r0, @-r15
	/* Store pointer to saved integer registers in caml_gc_regs */
	mov.l	.Lcaml_gc_regs, r10
	mov.l	r15, @r10
	/* Save non-callee-save float registers */
	fmov.s	fr14, @-r15	/* "Most significant" bits at higher address */
	fmov.s	fr15, @-r15
	fmov.s	fr12, @-r15
	fmov.s	fr13, @-r15
	fmov.s	fr10, @-r15
	fmov.s	fr11, @-r15
	fmov.s	fr8, @-r15
	fmov.s	fr9, @-r15
	fmov.s	fr6, @-r15
	fmov.s	fr7, @-r15
	fmov.s	fr4, @-r15
	fmov.s	fr5, @-r15
	fmov.s	fr2, @-r15
	fmov.s	fr3, @-r15
	fmov.s	fr0, @-r15
	fmov.s	fr1, @-r15
	/* Save current allocation pointer (ARM port says for debugging
	   purposes) */
	mov.l	.Lcaml_young_ptr, r10
	mov.l	alloc_ptr, @r10
	/* Save trap pointer in case an exception is raised during GC */
	mov.l	.Lcaml_exception_pointer, r10
	/* Call the garbage collector */
	mov.l	.Lcaml_garbage_collection, r11
	jsr	@r11
	mov.l	trap_ptr, @r10	! delay slot
	/* Restore the registers from the stack */
	fmov.s	@r15+, fr1
	fmov.s	@r15+, fr0
	fmov.s	@r15+, fr3
	fmov.s	@r15+, fr2
	fmov.s	@r15+, fr5
	fmov.s	@r15+, fr4
	fmov.s	@r15+, fr7
	fmov.s	@r15+, fr6
	fmov.s	@r15+, fr9
	fmov.s	@r15+, fr8
	fmov.s	@r15+, fr11
	fmov.s	@r15+, fr10
	fmov.s	@r15+, fr13
	fmov.s	@r15+, fr12
	fmov.s	@r15+, fr15
	fmov.s	@r15+, fr14
	mov.l	@r15+, r0
	mov.l	@r15+, r1
	mov.l	@r15+, r2
	mov.l	@r15+, r3
	mov.l	@r15+, r4
	mov.l	@r15+, r5
	mov.l	@r15+, r6
	mov.l	@r15+, r7
	mov.l	@r15+, r8
	mov.l	@r15+, r9
	/* Reload return address */
	mov.l	.Lcaml_last_return_address, r10
	lds.l	@r10+, pr
	/* Say that we are back into OCaml code */
	mov	#0, alloc_ptr
	mov.l	alloc_ptr, @-r10
	/* Reload new allocation pointer and allocation limit */
	mov.l	.Lcaml_young_ptr, r10
	mov.l	@r10, alloc_ptr
	mov.l	.Lcaml_young_limit, alloc_limit
	/* Return to caller (note: must preserve PR, so don't use rts!) */
	mov.l	@r15+, r11
	jmp	@r11
	nop	! delay slot

	.global caml_smodsi3
caml_smodsi3:
	mov.l	.L__sdivsi3_i4i, r3
	mov	r4, r8
	jsr	@r3
	mov	r5, r9

	mul.l	r9, r0
	sts	macl, r1
	sub	r1, r8

	rts
	mov	r8, r0	! delay slot

	.align 2
.L__sdivsi3_i4i:	.long	__sdivsi3_i4i

	/* Not sure what's special about this function which means it needs an
	   assembly implementation? */
	.global	caml_ml_array_bound_error
caml_ml_array_bound_error:
	mov.l	.Lcaml_array_bound_error, r11
	bsr	caml_c_call
	nop	! delay slot

/* Start the OCaml program */
	.global	caml_start_program
caml_start_program:
	mov.l	.Lcaml_program, r10

/* Code shared with caml_callback* */
/* Address of OCaml code to call is in r10 */
/* Arguments to the OCaml code are in r0-r3 */

jump_to_caml:
	/* Save return address and callee-save (C ABI) registers */
	sts.l	pr, @-r15
	mov.l	r14, @-r15
	mov.l	r13, @-r15
	mov.l	r12, @-r15
	mov.l	r11, @-r15
	mov.l	r10, @-r15
	mov.l	r9, @-r15
	mov.l	r8, @-r15
	fmov.s	fr14, @-r15
	fmov.s	fr15, @-r15
	fmov.s	fr12, @-r15
	fmov.s	fr13, @-r15
	/* Setup a callback link on the stack */
	add	#-12, r15
	mov.l	.Lcaml_bottom_of_stack, r8
	mov.l	@r8, r8
	mov.l	r8, @r15
	mov.l	.Lcaml_last_return_address, r8
	mov.l	@r8, r8
	mov.l	r8, @(4,r15)
	mov.l	.Lcaml_gc_regs, r8
	mov.l	@r8, r8
	mov.l	r8, @(8,r15)
	/* Setup a trap frame to catch exceptions escaping the OCaml code */
	add	#-8, r15
	mov.l	.Lcaml_exception_pointer, r8
	mov.l	@r8, r8
	mov.l	r8, @r15
	mov.l	.Ltrap_handler, r8
	mov.l	r8, @(4,r15)
	mov	r15, trap_ptr
	/* Reload allocation pointers */
	mov.l	.Lcaml_young_ptr, r8
	mov.l	@r8, alloc_ptr
	mov.l	.Lcaml_young_limit, alloc_limit
	/* We are back into OCaml code */
	mov.l	.Lcaml_last_return_address, r8
	mov	#0, r9
	/* Call the OCaml code */
	jsr	@r10
	mov.l	r9, @r8		! delay slot
caml_retaddr:
	/* Pop the trap frame, restoring caml_exception_pointer */
	mov.l	.Lcaml_exception_pointer, r8
	mov.l	@r15, r9
	mov.l	r9, @r8
	add	#8, r15
	/* Pop the callback link, restoring the global variables */
return_result:
	mov.l	.Lcaml_bottom_of_stack, r8
	mov.l	@r15, r9
	mov.l	r9, @r8
	mov.l	.Lcaml_last_return_address, r8
	mov.l	@(4,r15), r9
	mov.l	r9, @r8
	mov.l	.Lcaml_gc_regs, r8
	mov.l	@(8,r15), r9
	mov.l	r9, @r8
	add	#12, r15
	/* Update allocation pointer */
	mov.l	.Lcaml_young_ptr, r8
	mov.l	alloc_ptr, @r8
	/* Reload callee-save registers and return */
	fmov.s	@r15+, fr13
	fmov.s	@r15+, fr12
	fmov.s	@r15+, fr15
	fmov.s	@r15+, fr14
	mov.l	@r15+, r8
	mov.l	@r15+, r9
	mov.l	@r15+, r10
	mov.l	@r15+, r11
	mov.l	@r15+, r12
	mov.l	@r15+, r13
	mov.l	@r15+, r14
	lds.l	@r15+, pr
	rts
	nop	! delay slot

	/* The trap handler */
trap_handler:
	/* Save exception pointer (safe to use r8 here?) */
	mov.l	.Lcaml_exception_pointer, r8
	mov.l	trap_ptr, @r8
	bra	return_result
	/* Encode exception bucket as an exception result (FIXME? Don't
	   understand this.) */
	or	#2, r0	! delay slot

/* Raise an exception from C */

	.global	caml_raise_exception
caml_raise_exception:
	/* Reload OCaml allocation pointers */
	mov.l	.Lcaml_young_ptr, r1
	mov.l	@r1, alloc_ptr
	mov.l	.Lcaml_young_limit, alloc_limit
	/* Say we're back into OCaml */
	mov.l	.Lcaml_last_return_address, r1
	mov	#0, r2
	mov.l	r2, @r1
	/* Cut stack at current trap handler */
	mov.l	.Lcaml_exception_pointer, r1
	mov.l	@r1, r15
	/* Pop previous handler and addr of trap, and jump to it */
	mov.l	@r15+, trap_ptr
	mov.l	@r15+, r11
	jmp	@r11
	nop	! delay slot

/* Callback from C to OCaml */

	.global	caml_callback_exn
caml_callback_exn:
	/* Args are r4 = closure, r5 = first arg. */
	mov	r5, r0		/* r0 = first arg */
	mov	r4, r1		/* r1 = closure environment */
	bra	jump_to_caml
	mov.l	@r4, r10	/* delay slot, load code pointer */

	.global	caml_callback2_exn
caml_callback2_exn:
	/* Args are r4 = closure, r5 = arg1, r6 = arg2. */
	mov	r5, r0		/* r0 = first arg */
	mov	r6, r1		/* r1 = second arg */
	mov.l	.Lcaml_apply2, r10
	bra	jump_to_caml
	mov	r4, r2		/* delay slot, r2 = closure environment */
	
	.global caml_callback3_exn
caml_callback3_exn:
	/* Args are r4 = closure, r5 = arg1, r6 = arg2, r7 = arg3. */
	mov	r5, r0		/* r0 = first arg */
	mov	r6, r1		/* r1 = second arg */
	mov	r7, r2		/* r2 = third arg */
	mov.l	.Lcaml_apply3, r10
	bra	jump_to_caml
	mov	r4, r3		/* delay slot, r3 = closure environment */

	.align 2
.Lcaml_last_return_address:	.long	caml_last_return_address
.Lcaml_young_ptr:		.long	caml_young_ptr
.Lcaml_young_limit:		.long	caml_young_limit
.Lcaml_exception_pointer:	.long	caml_exception_pointer
.Lcaml_bottom_of_stack:		.long	caml_bottom_of_stack
.Lcaml_requested_size:		.long	caml_requested_size
.Lcaml_gc_regs:			.long	caml_gc_regs
.Lcaml_garbage_collection:	.long	caml_garbage_collection
.Lcaml_array_bound_error:	.long	caml_array_bound_error
.Lcaml_program:			.long	caml_program
.Ltrap_handler:			.long	trap_handler
.Lcaml_apply2:			.long	caml_apply2
.Lcaml_apply3:			.long	caml_apply3

	.data
caml_requested_size:		.long	0

	.global	caml_system__frametable
caml_system__frametable:
	.long	1		/* one descriptor */
	.long	caml_retaddr	/* return address into callback */
	.short	-1		/* negative frame size => use callback link */
	.short	0		/* no roots */
	.align	2

