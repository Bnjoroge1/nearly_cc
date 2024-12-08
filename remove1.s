
	.section .text

	.globl main
main:
	pushq    %rbp                /* enter    $0 */
	movq     %rsp, %rbp
	subq     $192, %rsp
	movl     $1000000000, -144(%rbp) /* mov_l    vr16, $1000000000 */
	movq     -144(%rbp), %r10    /* mov_q    vr10, vr16 */
	movq     %r10, -192(%rbp)
	movq     $0, -136(%rbp)      /* mov_q    vr17, $0 */
	movq     -136(%rbp), %r10    /* mov_q    vr11, vr17 */
	movq     %r10, -184(%rbp)
	movq     $1, -128(%rbp)      /* mov_q    vr18, $1 */
	movq     -128(%rbp), %r10    /* mov_q    vr12, vr18 */
	movq     %r10, -176(%rbp)
	movq     $0, -120(%rbp)      /* mov_q    vr19, $0 */
	movq     -120(%rbp), %r10    /* mov_q    vr15, vr19 */
	movq     %r10, -152(%rbp)
	movq     $2, -112(%rbp)      /* mov_q    vr20, $2 */
	movq     -112(%rbp), %r10    /* mov_q    vr14, vr20 */
	movq     %r10, -160(%rbp)
	jmp      .L1                 /* jmp      .L1 */
.L0:
	movl     -176(%rbp), %r10d   /* add_l    vr21, vr12, vr11 */
	addl     -184(%rbp), %r10d
	movl     %r10d, -104(%rbp)
	movq     -104(%rbp), %r10    /* mov_q    vr13, vr21 */
	movq     %r10, -168(%rbp)
	movq     $1000000, -96(%rbp) /* mov_q    vr22, $1000000 */
	movl     -168(%rbp), %r10d   /* cmpgt_l  vr23, vr13, vr22 */
	cmpl     -96(%rbp), %r10d
	setg     %r10b
	movzbl   %r10b, %r11d
	movl     %r11d, -88(%rbp)
	cmpl     $0, -88(%rbp)       /* cjmp_f   vr23, .L2 */
	je       .L2
	movq     $1000000, -80(%rbp) /* mov_q    vr24, $1000000 */
	movl     -168(%rbp), %r10d   /* sub_l    vr25, vr13, vr24 */
	subl     -80(%rbp), %r10d
	movl     %r10d, -72(%rbp)
	movq     -72(%rbp), %r10     /* mov_q    vr13, vr25 */
	movq     %r10, -168(%rbp)
.L2:
	movq     -176(%rbp), %r10    /* mov_q    vr11, vr12 */
	movq     %r10, -184(%rbp)
	movq     -168(%rbp), %r10    /* mov_q    vr12, vr13 */
	movq     %r10, -176(%rbp)
	movl     -152(%rbp), %r10d   /* add_l    vr26, vr15, vr12 */
	addl     -176(%rbp), %r10d
	movl     %r10d, -64(%rbp)
	movq     -64(%rbp), %r10     /* mov_q    vr15, vr26 */
	movq     %r10, -152(%rbp)
	movq     $1000000, -56(%rbp) /* mov_q    vr27, $1000000 */
	movl     -152(%rbp), %r10d   /* cmpgt_l  vr28, vr15, vr27 */
	cmpl     -56(%rbp), %r10d
	setg     %r10b
	movzbl   %r10b, %r11d
	movl     %r11d, -48(%rbp)
	cmpl     $0, -48(%rbp)       /* cjmp_f   vr28, .L3 */
	je       .L3
	movq     $1000000, -40(%rbp) /* mov_q    vr29, $1000000 */
	movl     -152(%rbp), %r10d   /* sub_l    vr30, vr15, vr29 */
	subl     -40(%rbp), %r10d
	movl     %r10d, -32(%rbp)
	movq     -32(%rbp), %r10     /* mov_q    vr15, vr30 */
	movq     %r10, -152(%rbp)
.L3:
	movq     $1, -24(%rbp)       /* mov_q    vr31, $1 */
	movl     -160(%rbp), %r10d   /* add_l    vr32, vr14, vr31 */
	addl     -24(%rbp), %r10d
	movl     %r10d, -16(%rbp)
	movq     -16(%rbp), %r10     /* mov_q    vr14, vr32 */
	movq     %r10, -160(%rbp)
.L1:
	movl     -160(%rbp), %r10d   /* cmplte_l vr33, vr14, vr10 */
	cmpl     -192(%rbp), %r10d
	setle    %r10b
	movzbl   %r10b, %r11d
	movl     %r11d, -8(%rbp)
	cmpl     $0, -8(%rbp)        /* cjmp_t   vr33, .L0 */
	jne      .L0
	movl     -152(%rbp), %edi    /* mov_l    vr1, vr15 */
	call     print_i64           /* call     print_i64 */
	call     print_nl            /* call     print_nl */
	movl     -176(%rbp), %edi    /* mov_l    vr1, vr12 */
	call     print_i64           /* call     print_i64 */
	call     print_nl            /* call     print_nl */
	movl     $0, 0(%rbp)         /* mov_l    vr34, $0 */
	movl     0(%rbp), %eax       /* mov_l    vr0, vr34 */
	jmp      .Lmain_return       /* jmp      .Lmain_return */
.Lmain_return:
	addq     $192, %rsp          /* leave    $0 */
	popq     %rbp
	ret                          /* ret       */
