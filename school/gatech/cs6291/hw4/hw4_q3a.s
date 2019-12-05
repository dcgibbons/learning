 ## Target: VEX 1 cluster (big endian)
.comment ""
.comment "Copyright (C) 1990-2010 Hewlett-Packard Company"
.comment "VEX C compiler version 3.43 (20110131 release)"
.comment ""
.comment "-dir /home/chadwick/vex-3.43 -S -O0 -H0 -fmm=hw4_q3a.cfg"
.sversion 3.43
.rta 2
.section .bss
.align 32
.section .data
.align 32
 ## Begin main
.section .text
.proc
.entry caller, sp=$r0.1, rl=$l0.0, asize=-64, arg()
main::
.trace 1
	c0    add $r0.1 = $r0.1, (-0x40)
	c0    mov $r0.5 = 8   ## 8(SI32)
;;								## 0
	c0    add $r0.4 = $r0.1, 0x10   ## bblock 0, line 6,  t3,  t25,  offset(vals?1.2)=0x10(P32)
	c0    mov $r0.3 = (_?1PACKET.1 + 0)   ## addr(vals.?1AUTOINIT?1.2)(P32)
	c0    stw 0x18[$r0.1] = $l0.0  ## spill ## t24
;;								## 1
.call _bcopy, caller, arg($r0.3:u32,$r0.4:u32,$r0.5:s32), ret()
	c0    call $l0.0 = _bcopy   ## bblock 0, line 6,  raddr(_bcopy)(P32),  addr(vals.?1AUTOINIT?1.2)(P32),  t3,  8(SI32)
	c0    stw 0x1c[$r0.1] = $r0.4  ## spill ## t3
;;								## 2
	c0    mov $r0.3 = (_?1STRINGPACKET.1 + 0)   ## addr(_?1STRINGVAR.1)(P32)
	c0    ldw $r0.2 = 0x1c[$r0.1]  ## restore ## t3
	      xnop 2
;;								## 5
	c0    ldw $r0.6 = 0[$r0.2]   ## bblock 0, line 7, t7, t3
;;								## 6
	c0    ldw $r0.2 = 4[$r0.2]   ## bblock 0, line 8, t6, t3
	      xnop 8
;;								## 15
	c0    add $r0.7 = $r0.6, $r0.2   ## bblock 0, line 10,  t35,  t7,  t6
;;								## 16
.call printf, caller, arg($r0.3:u32,$r0.4:s32,$r0.5:s32), ret($r0.3:s32)
	c0    add $r0.4 = $r0.7, 3   ## bblock 0, line 12,  t15,  t35,  3(SI32)
	c0    call $l0.0 = printf   ## bblock 0, line 13,  raddr(printf)(P32),  addr(_?1STRINGVAR.1)(P32),  t15,  t35
	c0    stw 0x20[$r0.1] = $r0.7  ## spill ## t35
	c0    mov $r0.5 = $r0.7   ## t35
;;								## 17
	c0    mov $r0.3 = (_?1STRINGPACKET.2 + 0)   ## addr(_?1STRINGVAR.2)(P32)
	c0    ldw $r0.7 = 0x20[$r0.1]  ## restore ## t35
	      xnop 2
;;								## 20
	c0    add $r0.4 = $r0.7, 5   ## bblock 1, line 14,  t39,  t35,  5(SI32)
;;								## 21
.call printf, caller, arg($r0.3:u32,$r0.4:s32), ret($r0.3:s32)
	c0    call $l0.0 = printf   ## bblock 1, line 15,  raddr(printf)(P32),  addr(_?1STRINGVAR.2)(P32),  t39
	c0    stw 0x24[$r0.1] = $r0.4  ## spill ## t39
;;								## 22
	c0    ldw $l0.0 = 0x18[$r0.1]  ## restore ## t24
;;								## 23
	c0    ldw $r0.3 = 0x24[$r0.1]  ## restore ## t39
	      xnop 2
;;								## 26
.return ret($r0.3:s32)
	c0    return $r0.1 = $r0.1, (0x40), $l0.0   ## bblock 2, line 17,  t25,  ?2.1?2auto_size(I32),  t24
;;								## 27
.endp
.section .bss
.section .data
_?1PACKET.1:
    .data4 10
    .data4 7
_?1STRINGPACKET.2:
    .data1 100
    .data1 61
    .data1 37
    .data1 100
    .data1 0
.skip 3
_?1STRINGPACKET.1:
    .data1 97
    .data1 61
    .data1 37
    .data1 100
    .data1 32
    .data1 100
    .data1 61
    .data1 37
    .data1 100
    .data1 10
    .data1 0
.equ ?2.1?2scratch.0, 0x0
.equ _?1PACKET.2, 0x10
.equ ?2.1?2ras_p, 0x18
.equ ?2.1?2spill_p, 0x1c
.section .data
.section .text
.equ ?2.1?2auto_size, 0x40
 ## End main
.section .bss
.section .data
.section .data
.section .text
.import _bcopy
.type _bcopy,@function
.import printf
.type printf,@function
