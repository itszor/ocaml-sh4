/* SH4 runtime support routines */

#define trap_ptr r14
#define alloc_ptr r12
#define alloc_limit r13

	.text
	.global caml_call_gc
caml_call_gc:
	/* FIXME: Not implemented. */
	rts

	/* Call a C function from OCaml. Function to call is in r11.
	   The callee preserves r8-r15. r8-r11 are good to use as
	   temporaries. Modelled on ARM implementation. */

	.global caml_c_call
caml_c_call:
	/* Return address in callee-save register. */
	sts	pr, r8

	/* Record lowest stack add
	mov.l	.Lcaml_bottom_of_stack, r9
	mov.l	.Lcaml_last_return_address, r10
	add	#4, r10
	sts.l	pr, @-r10
	mov.l	r15, @r9

	/* Make the exception handler and alloc ptr available to the C code */
	mov.l	.Lcaml_exception_ptr, r9
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
	lds	r8, pr
	rts
	/* r10 is still caml_last_return_address */
	mov.l	r9, @r10	! delay slot

	.global caml_allocN
caml_allocN:
	/* Argh. Complicated. */
	rts
	nop	! delay slot

.Lcaml_last_return_address	.long	caml_last_return_address
.Lcaml_young_ptr		.long	caml_young_ptr
.Lcaml_exception_pointer	.long	caml_exception_pointer
.Lcaml_bottom_of_stack		.long	caml_bottom_of_stack
