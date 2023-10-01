segment	.text
align	4
_L1:
	push	ebp
	mov	ebp, esp
	sub	esp, 0
segment	.rodata
align	4
_L2:
	db	"ola", 0
align	4
segment	.text
	push	dword $_L2
	call	prints
	add	esp, 4
	call	println
	leave
	ret
segment	.data
align	4
f:
	dd	_L1
segment	.text
align	4
global	_main:function
_main:
	push	ebp
	mov	ebp, esp
	sub	esp, 0
	push	dword $f
	pop	eax
	push	dword [eax]
	call	f
	push	eax
	push	dword 0
	pop	eax
	leave
	ret
	push	dword 0
	pop	eax
	leave
	ret
extern	readi
extern	readd
extern	printi
extern	printd
extern	prints
extern	println
