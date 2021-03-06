dnl  IBM POWER mpn_add_n -- Add two limb vectors of equal, non-zero length.

dnl  Copyright 1992, 1994, 1995, 1996, 1999, 2000, 2001, 2005 Free Software
dnl  Foundation, Inc.

dnl  This file is part of the GNU MP Library.

dnl  The GNU MP Library is free software; you can redistribute it and/or modify
dnl  it under the terms of the GNU Lesser General Public License as published
dnl  by the Free Software Foundation; either version 2.1 of the License, or (at
dnl  your option) any later version.

dnl  The GNU MP Library is distributed in the hope that it will be useful, but
dnl  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
dnl  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
dnl  License for more details.

dnl  You should have received a copy of the GNU Lesser General Public License
dnl  along with the GNU MP Library; see the file COPYING.LIB.  If not, write
dnl  to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
dnl  Boston, MA 02110-1301, USA.


dnl  INPUT PARAMETERS
dnl  res_ptr	r3
dnl  s1_ptr	r4
dnl  s2_ptr	r5
dnl  size	r6

include(`../config.m4')

ASM_START()
PROLOGUE(mpn_add_n)
	andil.	10,6,1		C odd or even number of limbs?
	l	8,0(4)		C load least significant s1 limb
	l	0,0(5)		C load least significant s2 limb
	cal	3,-4(3)		C offset res_ptr, it's updated before it's used
	sri	10,6,1		C count for unrolled loop
	a	7,0,8		C add least significant limbs, set cy
	mtctr	10		C copy count into CTR
	beq	0,Leven		C branch if even # of limbs (# of limbs >= 2)

C We have an odd # of limbs.  Add the first limbs separately.
	cmpi	1,10,0		C is count for unrolled loop zero?
	bc	4,6,L1		C bne cr1,L1 (misassembled by gas)
	st	7,4(3)
	aze	3,10		C use the fact that r10 is zero...
	br			C return

C We added least significant limbs.  Now reload the next limbs to enter loop.
L1:	lu	8,4(4)		C load s1 limb and update s1_ptr
	lu	0,4(5)		C load s2 limb and update s2_ptr
	stu	7,4(3)
	ae	7,0,8		C add limbs, set cy
Leven:	lu	9,4(4)		C load s1 limb and update s1_ptr
	lu	10,4(5)		C load s2 limb and update s2_ptr
	bdz	Lend		C If done, skip loop

Loop:	lu	8,4(4)		C load s1 limb and update s1_ptr
	lu	0,4(5)		C load s2 limb and update s2_ptr
	ae	11,10,9		C add previous limbs with cy, set cy
	stu	7,4(3)		C
	lu	9,4(4)		C load s1 limb and update s1_ptr
	lu	10,4(5)		C load s2 limb and update s2_ptr
	ae	7,0,8		C add previous limbs with cy, set cy
	stu	11,4(3)		C
	bdn	Loop		C decrement CTR and loop back

Lend:	ae	11,10,9		C add limbs with cy, set cy
	st	7,4(3)		C
	st	11,8(3)		C
	lil	3,0		C load cy into ...
	aze	3,3		C ... return value register
	br
EPILOGUE(mpn_add_n)
