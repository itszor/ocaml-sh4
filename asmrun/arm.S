/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: arm.S,v 1.16 2008/02/29 14:21:21 doligez Exp $ */

/* Linux/BSD with ELF binaries and Solaris do not prefix identifiers with _.
   Linux/BSD with a.out binaries and NextStep do.
   Copied from asmrun/i386.S */

#if defined(SYS_solaris)
#define CONCAT(a,b) a/**/b
#else
#define CONCAT(a,b) a##b
#endif

#if defined(SYS_linux_elf) || defined(SYS_bsd_elf) \
 || defined(SYS_solaris) || defined(SYS_beos) || defined(SYS_gnu)
#define G(x) x
#define LBL(x) CONCAT(.L,x)
#else
#define G(x) CONCAT(_,x)
#define LBL(x) CONCAT(L,x)
#endif

/* Asm part of the runtime system, ARM processor */

#define trap_ptr        r11
#define alloc_ptr       r8
#define alloc_limit     r9
#define sp              r13
#define lr              r14
#define pc              r15

	.arch armv5t
	.fpu vfp
        .text

/* Allocation functions and GC interface */

        .global G(caml_call_gc)
G(caml_call_gc):
    /* Record return address */
    /* We can use r10 as a temp reg since it's not live here */
        ldr     r10, LBL(caml_last_return_address)
        str     lr, [r10, #0]
    /* Branch to shared GC code */
        bl      LBL(invoke_gc)
    /* Restart allocation sequence (4 instructions before) */
        sub     lr, lr, #16
        mov     pc, lr

        .global G(caml_alloc1)
G(caml_alloc1):
        ldr     r10, [alloc_limit, #0]
        sub     alloc_ptr, alloc_ptr, #8
        cmp     alloc_ptr, r10
        movcs   pc, lr                /* Return if alloc_ptr >= alloc_limit */
    /* Record return address */
        ldr     r10, LBL(caml_last_return_address)
        str     lr, [r10, #0]
    /* Invoke GC */
        bl      LBL(invoke_gc)
    /* Try again */
        b       G(caml_alloc1)

        .global G(caml_alloc2)
G(caml_alloc2):
        ldr     r10, [alloc_limit, #0]
        sub     alloc_ptr, alloc_ptr, #12
        cmp     alloc_ptr, r10
        movcs   pc, lr                /* Return if alloc_ptr >= alloc_limit */
    /* Record return address */
        ldr     r10, LBL(caml_last_return_address)
        str     lr, [r10, #0]
    /* Invoke GC */
        bl      LBL(invoke_gc)
    /* Try again */
        b       G(caml_alloc2)

        .global G(caml_alloc3)
G(caml_alloc3):
        ldr     r10, [alloc_limit, #0]
        sub     alloc_ptr, alloc_ptr, #16
        cmp     alloc_ptr, r10
        movcs   pc, lr                /* Return if alloc_ptr >= alloc_limit */
    /* Record return address */
        ldr     r10, LBL(caml_last_return_address)
        str     lr, [r10, #0]
    /* Invoke GC */
        bl      LBL(invoke_gc)
    /* Try again */
        b       G(caml_alloc3)

        .global G(caml_allocN)
G(caml_allocN):
        str     r12, [sp, #-4]!
        ldr     r12, [alloc_limit, #0]
        sub     alloc_ptr, alloc_ptr, r10
        cmp     alloc_ptr, r12
        ldr     r12, [sp], #4
        movcs   pc, lr                /* Return if alloc_ptr >= alloc_limit */
    /* Record return address and desired size */
        ldr     alloc_limit, LBL(caml_last_return_address)
        str     lr, [alloc_limit, #0]
        ldr     alloc_limit, LBL(Lcaml_requested_size)
        str     r10, [alloc_limit, #0]
    /* Invoke GC */
        bl      LBL(invoke_gc)
    /* Try again */
        ldr     r10, LBL(Lcaml_requested_size)
        ldr     r10, [r10, #0]
        b       G(caml_allocN)

/* Shared code to invoke the GC */
LBL(invoke_gc):
    /* Record lowest stack address */
        ldr     r10, LBL(caml_bottom_of_stack)
        str     sp, [r10, #0]
    /* Save integer registers and return address on stack */
        stmfd   sp!, {r0,r1,r2,r3,r4,r5,r6,r7,r10,r12,lr}
    /* Store pointer to saved integer registers in caml_gc_regs */
        ldr     r10, LBL(caml_gc_regs)
        str     sp, [r10, #0]
    /* Save non-callee-save float registers */
        sub     sp, sp, #64
        fstd    d0, [sp, #56]
        fstd    d1, [sp, #48]
        fstd    d2, [sp, #40]
        fstd    d3, [sp, #32]
        fstd    d4, [sp, #24]
        fstd    d5, [sp, #16]
        fstd    d6, [sp, #8]
        fstd    d7, [sp, #0]
    /* Save current allocation pointer for debugging purposes */
        ldr     r10, LBL(caml_young_ptr)
        str     alloc_ptr, [r10, #0]
    /* Save trap pointer in case an exception is raised during GC */
        ldr     r10, LBL(caml_exception_pointer)
        str     trap_ptr, [r10, #0]
    /* Call the garbage collector */
        bl      G(caml_garbage_collection)
    /* Restore the registers from the stack */
        fldd    d7, [sp, #0]
        fldd    d6, [sp, #8]
        fldd    d5, [sp, #16]
        fldd    d4, [sp, #24]
        fldd    d3, [sp, #32]
        fldd    d2, [sp, #40]
        fldd    d1, [sp, #48]
        fldd    d0, [sp, #56]
        add     sp, sp, #64
        ldmfd   sp!, {r0,r1,r2,r3,r4,r5,r6,r7,r10,r12}
    /* Reload return address */
        ldr     r10, LBL(caml_last_return_address)
        ldr     lr, [r10, #0]
    /* Say that we are back into Caml code */
        mov     alloc_ptr, #0
        str     alloc_ptr, [r10, #0]
    /* Reload new allocation pointer and allocation limit */
        ldr     r10, LBL(caml_young_ptr)
        ldr     alloc_ptr, [r10, #0]
        ldr     alloc_limit, LBL(caml_young_limit)
    /* Return to caller */
        ldmfd   sp!, {pc}

/* Call a C function from Caml */
/* Function to call is in r10 */

        .global G(caml_c_call)
G(caml_c_call):
    /* Preserve return address in callee-save register r4 */
        mov     r4, lr
    /* Record lowest stack address and return address */
        ldr     r5, LBL(caml_last_return_address)
        ldr     r6, LBL(caml_bottom_of_stack)
        str     lr, [r5, #0]
        str     sp, [r6, #0]
    /* Make the exception handler and alloc ptr available to the C code */
        ldr     r6, LBL(caml_young_ptr)
        ldr     r7, LBL(caml_exception_pointer)
        str     alloc_ptr, [r6, #0]
        str     trap_ptr, [r7, #0]
    /* Call the function */
        mov     lr, pc
        mov     pc, r10
    /* Reload alloc ptr */
        ldr     alloc_ptr, [r6, #0]    /* r6 still points to caml_young_ptr */
    /* Say that we are back into Caml code */
        mov     r6, #0
        str     r6, [r5, #0] /* r5 still points to caml_last_return_address */
    /* Return */
        mov     pc, r4

/* Start the Caml program */

        .global G(caml_start_program)
G(caml_start_program):
        ldr     r10, LBL(caml_program)

/* Code shared with caml_callback* */
/* Address of Caml code to call is in r10 */
/* Arguments to the Caml code are in r0...r3 */

LBL(jump_to_caml):
    /* Save return address and callee-save registers */
        stmfd   sp!, {r4,r5,r6,r7,r8,r9,r11,lr}
        sub     sp, sp, #64
        fstd    d15, [sp, #56]
        fstd    d14, [sp, #48]
        fstd    d13, [sp, #40]
        fstd    d12, [sp, #32]
        fstd    d11, [sp, #24]
        fstd    d10, [sp, #16]
        fstd    d9, [sp, #8]
        fstd    d8, [sp, #0]
    /* Setup a callback link on the stack */
        sub     sp, sp, #(4*3)
        ldr     r4, LBL(caml_bottom_of_stack)
        ldr     r4, [r4, #0]
        str     r4, [sp, #0]
        ldr     r4, LBL(caml_last_return_address)
        ldr     r4, [r4, #0]
        str     r4, [sp, #4]
        ldr     r4, LBL(caml_gc_regs)
        ldr     r4, [r4, #0]
        str     r4, [sp, #8]
    /* Setup a trap frame to catch exceptions escaping the Caml code */
        sub     sp, sp, #(4*2)
        ldr     r4, LBL(caml_exception_pointer)
        ldr     r4, [r4, #0]
        str     r4, [sp, #0]
        ldr     r4, LBL(Ltrap_handler)
        str     r4, [sp, #4]
        mov     trap_ptr, sp
    /* Reload allocation pointers */
        ldr     r4, LBL(caml_young_ptr)
        ldr     alloc_ptr, [r4, #0]
        ldr     alloc_limit, LBL(caml_young_limit)
    /* We are back into Caml code */
        ldr     r4, LBL(caml_last_return_address)
        mov     r5, #0
        str     r5, [r4, #0]
    /* Call the Caml code */
        mov     lr, pc
        mov     pc, r10
LBL(caml_retaddr):
    /* Pop the trap frame, restoring caml_exception_pointer */
        ldr     r4, LBL(caml_exception_pointer)
        ldr     r5, [sp, #0]
        str     r5, [r4, #0]
        add     sp, sp, #(2 * 4)
    /* Pop the callback link, restoring the global variables */
LBL(return_result):
        ldr     r4, LBL(caml_bottom_of_stack)
        ldr     r5, [sp, #0]
        str     r5, [r4, #0]
        ldr     r4, LBL(caml_last_return_address)
        ldr     r5, [sp, #4]
        str     r5, [r4, #0]
        ldr     r4, LBL(caml_gc_regs)
        ldr     r5, [sp, #8]
        str     r5, [r4, #0]
        add     sp, sp, #(4*3)
    /* Update allocation pointer */
        ldr     r4, LBL(caml_young_ptr)
        str     alloc_ptr, [r4, #0]
    /* Reload callee-save registers and return */
        fldd    d8, [sp, #0]
        fldd    d9, [sp, #8]
        fldd    d10, [sp, #16]
        fldd    d11, [sp, #24]
        fldd    d12, [sp, #32]
        fldd    d13, [sp, #40]
        fldd    d14, [sp, #48]
        fldd    d15, [sp, #56]
        add     sp, sp, #64
        ldmfd   sp!, {r4,r5,r6,r7,r8,r9,r11,pc}

    /* The trap handler */
LBL(trap_handler):
    /* Save exception pointer */
        ldr     r4, LBL(caml_exception_pointer)
        str     trap_ptr, [r4, #0]
    /* Encode exception bucket as an exception result */
        orr     r0, r0, #2
    /* Return it */
        b       LBL(return_result)

/* Raise an exception from C */

        .global G(caml_raise_exception)
G(caml_raise_exception):
    /* Reload Caml allocation pointers */
        ldr     r1, LBL(caml_young_ptr)
        ldr     alloc_ptr, [r1, #0]
        ldr     alloc_limit, LBL(caml_young_limit)
    /* Say we're back into Caml */
        ldr     r1, LBL(caml_last_return_address)
        mov     r2, #0
        str     r2, [r1, #0]
    /* Cut stack at current trap handler */
        ldr     r1, LBL(caml_exception_pointer)
        ldr     sp, [r1, #0]
    /* Pop previous handler and addr of trap, and jump to it */
        ldmfd   sp!, {trap_ptr, pc}

/* Callback from C to Caml */

        .global G(caml_callback_exn)
G(caml_callback_exn):
    /* Initial shuffling of arguments (r0 = closure, r1 = first arg) */
        mov     r10, r0
        mov     r0, r1            /* r0 = first arg */
        mov     r1, r10           /* r1 = closure environment */
        ldr     r10, [r10, #0]    /* code pointer */
        b       LBL(jump_to_caml)

        .global G(caml_callback2_exn)
G(caml_callback2_exn):
    /* Initial shuffling of arguments (r0 = closure, r1 = arg1, r2 = arg2) */
        mov     r10, r0
        mov     r0, r1           /* r0 = first arg */
        mov     r1, r2           /* r1 = second arg */
        mov     r2, r10          /* r2 = closure environment */
        ldr     r10, LBL(caml_apply2)
        b       LBL(jump_to_caml)

        .global G(caml_callback3_exn)
G(caml_callback3_exn):
    /* Initial shuffling of arguments */
    /* (r0 = closure, r1 = arg1, r2 = arg2, r3 = arg3) */
        mov     r10, r0
        mov     r0, r1          /* r0 = first arg */
        mov     r1, r2          /* r1 = second arg */
        mov     r2, r3          /* r2 = third arg */
        mov     r3, r10         /* r3 = closure environment */
        ldr     r10, LBL(caml_apply3)
        b       LBL(jump_to_caml)

        .global G(caml_ml_array_bound_error)
G(caml_ml_array_bound_error):
    /* Load address of [caml_array_bound_error] in r10 */
        ldr     r10, LBL(caml_array_bound_error)
    /* Call that function */
        b       G(caml_c_call)

/* Global references */

LBL(caml_last_return_address):     .long G(caml_last_return_address)
LBL(caml_bottom_of_stack):         .long G(caml_bottom_of_stack)
LBL(caml_gc_regs):                 .long G(caml_gc_regs)
LBL(caml_young_ptr):               .long G(caml_young_ptr)
LBL(caml_young_limit):             .long G(caml_young_limit)
LBL(caml_exception_pointer):       .long G(caml_exception_pointer)
LBL(caml_program):                 .long G(caml_program)
LBL(Ltrap_handler):                .long LBL(trap_handler)
LBL(caml_apply2):                  .long G(caml_apply2)
LBL(caml_apply3):                  .long G(caml_apply3)
LBL(Lcaml_requested_size):         .long LBL(caml_requested_size)
LBL(caml_array_bound_error):       .long G(caml_array_bound_error)

.data
LBL(caml_requested_size):          .long 0

/* GC roots for callback */

        .data

        .global G(caml_system__frametable)
G(caml_system__frametable):
        .long   1               /* one descriptor */
        .long   LBL(caml_retaddr)  /* return address into callback */
        .short  -1              /* negative frame size => use callback link */
        .short  0               /* no roots */
        .align  2

#if defined(SYS_macosx)
        .text
        .global G(__stub__modsi3)
G(__stub__modsi3):
        b       LBL(__stub__modsi3)
        .global G(__stub__divsi3)
G(__stub__divsi3):
        b       LBL(__stub__divsi3)

        .section __TEXT,__picsymbolstub4,symbol_stubs,none,16
        .align 2
LBL(__stub__modsi3):
        .indirect_symbol G(__modsi3)
        ldr     ip, LBL(__stub__modsi3$slp)
LBL(__stub__modsi3$scv):
        add     ip, pc, ip
        ldr     pc, [ip, #0]
LBL(__stub__modsi3$slp):
        .long   LBL(__stub__modsi3$lazy_ptr) - (LBL(__stub__modsi3$scv) + 8)
        .lazy_symbol_pointer
LBL(__stub__modsi3$lazy_ptr):
        .indirect_symbol G(__modsi3)
        .long   dyld_stub_binding_helper

        .section __TEXT,__picsymbolstub4,symbol_stubs,none,16
        .align 2
LBL(__stub__divsi3):
        .indirect_symbol G(__divsi3)
        ldr     ip, LBL(__stub__divsi3$slp)
LBL(__stub__divsi3$scv):
        add     ip, pc, ip
        ldr     pc, [ip, #0]
LBL(__stub__divsi3$slp):
        .long   LBL(__stub__divsi3$lazy_ptr) - (LBL(__stub__divsi3$scv) + 8)
        .lazy_symbol_pointer
LBL(__stub__divsi3$lazy_ptr):
        .indirect_symbol G(__divsi3)
        .long   dyld_stub_binding_helper

        .subsections_via_symbols
#endif
