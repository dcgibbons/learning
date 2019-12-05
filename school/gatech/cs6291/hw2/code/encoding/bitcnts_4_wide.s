	c0    NOP
	c0    NOP
	c0    mov $r0.7 = $r0.0   ## bblock 0, line 11,  t21,  0(SI32)
	c0    cmpne $b0.0 = $r0.3, $r0.0   ## bblock 0, line 16,  t69(I1),  t20,  0(SI32)
;;								## 0
	c0    mov $r0.9 = 2   ## [spec] bblock 4, line 0,  t49,  2(I32)
	c0    mov $r0.6 = 2   ## [spec] bblock 4, line 0,  t45,  2(I32)
	c0    mov $r0.8 = 3   ## [spec] bblock 4, line 0,  t50,  3(I32)
	c0    mov $r0.4 = 3   ## [spec] bblock 4, line 0,  t46,  3(I32)
;;								## 1
	c0    mov $r0.10 = 1   ## [spec] bblock 4, line 0,  t48,  1(I32)
	c0    mov $r0.5 = 1   ## [spec] bblock 4, line 0,  t47,  1(I32)
	c0    mov $r0.11 = $r0.0   ## [spec] bblock 4, line 11,  t22,  0(SI32)
	c0    brf $b0.0, L0?3   ## bblock 0, line 16,  t69(I1)
;;								## 2
	c0    NOP
	c0    NOP
	c0    NOP
	c0    mov $r0.2 = $r0.3   ## t20
;;								## 3
	c0    add $r0.3 = $r0.2, -1   ## bblock 2, line 18,  t23,  t20,  -1(SI32)
	c0    NOP
	c0    add $r0.7 = $r0.7, 4   ## [spec] bblock 7, line 17-6,  t21,  t21,  4(SI32)
	c0    NOP
;;								## 0
	c0    NOP
	c0    NOP
	c0    and $r0.2 = $r0.2, $r0.3   ## bblock 2, line 18,  t25,  t20,  t23
	c0    NOP
;;								## 1
	c0    NOP
	c0    cmpeq $b0.0 = $r0.2, $r0.0   ## bblock 2, line 18,  t70(I1),  t25,  0(SI32)
	c0    add $r0.3 = $r0.2, -1   ## [spec] bblock 17, line 18-1,  t42,  t25,  -1(SI32)
	c0    NOP
;;								## 2
	c0    and $r0.2 = $r0.2, $r0.3   ## [spec] bblock 17, line 18-1,  t43,  t25,  t42
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 3
	c0    cmpeq $b0.0 = $r0.2, $r0.0   ## [spec] bblock 17, line 18-1,  t77(I1),  t43,  0(SI32)
	c0    add $r0.3 = $r0.2, -1   ## [spec] bblock 15, line 18-2,  t39,  t43,  -1(SI32)
	c0    NOP
	c0    br $b0.0, L2?3   ## bblock 2, line 18,  t70(I1)
;;								## 4
	c0    NOP
	c0    and $r0.2 = $r0.2, $r0.3   ## [spec] bblock 15, line 18-2,  t40,  t43,  t39
	c0    add $r0.11 = $r0.11, 4   ## [spec] bblock 5, line 17-7,  t22,  t22,  4(SI32)
	c0    NOP
;;								## 5
	c0    cmpeq $b0.0 = $r0.2, $r0.0   ## [spec] bblock 15, line 18-2,  t76(I1),  t40,  0(SI32)
	c0    NOP
	c0    add $r0.3 = $r0.2, -1   ## [spec] bblock 13, line 18-3,  t36,  t40,  -1(SI32)
	c0    br $b0.0, L3?3   ## bblock 17, line 18-1,  t77(I1)
;;								## 6
	c0    and $r0.2 = $r0.2, $r0.3   ## [spec] bblock 13, line 18-3,  t37,  t40,  t36
	c0    add $r0.5 = $r0.5, 4   ## [spec] bblock 7, line 0,  t47,  t47,  4(I32)
	c0    NOP
	c0    NOP
;;								## 7
	c0    cmpeq $b0.0 = $r0.2, $r0.0   ## [spec] bblock 13, line 18-3,  t75(I1),  t37,  0(SI32)
	c0    NOP
	c0    add $r0.3 = $r0.2, -1   ## [spec] bblock 11, line 18-4,  t33,  t37,  -1(SI32)
	c0    br $b0.0, L4?3   ## bblock 15, line 18-2,  t76(I1)
;;								## 8
	c0    and $r0.2 = $r0.2, $r0.3   ## [spec] bblock 11, line 18-4,  t34,  t37,  t33
	c0    NOP
	c0    add $r0.10 = $r0.10, 4   ## [spec] bblock 5, line 0,  t48,  t48,  4(I32)
	c0    NOP
;;								## 9
	c0    cmpeq $b0.0 = $r0.2, $r0.0   ## [spec] bblock 11, line 18-4,  t74(I1),  t34,  0(SI32)
	c0    add $r0.3 = $r0.2, -1   ## [spec] bblock 9, line 18-5,  t30,  t34,  -1(SI32)
	c0    NOP
	c0    br $b0.0, L5?3   ## bblock 13, line 18-3,  t75(I1)
;;								## 10
	c0    NOP
	c0    and $r0.2 = $r0.2, $r0.3   ## [spec] bblock 9, line 18-5,  t31,  t34,  t30
	c0    add $r0.6 = $r0.6, 4   ## [spec] bblock 7, line 0,  t45,  t45,  4(I32)
	c0    NOP
;;								## 11
	c0    cmpeq $b0.0 = $r0.2, $r0.0   ## [spec] bblock 9, line 18-5,  t73(I1),  t31,  0(SI32)
	c0    add $r0.3 = $r0.2, -1   ## [spec] bblock 7, line 18-6,  t27,  t31,  -1(SI32)
	c0    NOP
	c0    br $b0.0, L6?3   ## bblock 11, line 18-4,  t74(I1)
;;								## 12
	c0    and $r0.2 = $r0.2, $r0.3   ## [spec] bblock 7, line 18-6,  t26,  t31,  t27
	c0    NOP
	c0    NOP
	c0    add $r0.9 = $r0.9, 4   ## [spec] bblock 5, line 0,  t49,  t49,  4(I32)
;;								## 13
	c0    NOP
	c0    cmpeq $b0.0 = $r0.2, $r0.0   ## [spec] bblock 7, line 18-6,  t72(I1),  t26,  0(SI32)
	c0    add $r0.3 = $r0.2, -1   ## [spec] bblock 5, line 18-7,  t4,  t26,  -1(SI32)
	c0    br $b0.0, L7?3   ## bblock 9, line 18-5,  t73(I1)
;;								## 14
	c0    add $r0.4 = $r0.4, 4   ## bblock 7, line 0,  t46,  t46,  4(I32)
	c0    NOP
	c0    and $r0.2 = $r0.2, $r0.3   ## [spec] bblock 5, line 18-7,  t20,  t26,  t4
	c0    NOP
;;								## 15
	c0    cmpeq $b0.0 = $r0.2, $r0.0   ## [spec] bblock 5, line 18-7,  t71(I1),  t20,  0(SI32)
	c0    br $b0.0, L8?3   ## bblock 7, line 18-6,  t72(I1)
	c0    NOP
	c0    NOP
;;								## 16
	c0    NOP
	c0    add $r0.8 = $r0.8, 4   ## bblock 5, line 0,  t50,  t50,  4(I32)
	c0    NOP
	c0    NOP
;;								## 17
	c0    NOP
	c0    NOP
	c0    br $b0.0, L9?3   ## bblock 5, line 18-7,  t71(I1)
	c0    NOP
;;
	c0    goto L1?3 ## goto
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 18
	c0    NOP
	c0    mov $r0.5 = $r0.7   ## bblock 6, line 17-6,  t29,  t21
	c0    goto L10?3 ## goto
	c0    NOP
;;								## 0
	c0    return $r0.1 = $r0.1, (0x0), $l0.0   ## bblock 1, line 19,  t9,  ?2.1?2auto_size(I32),  t8
	c0    NOP
	c0    NOP
	c0    mov $r0.3 = $r0.7   ## t21
;;								## 0
	c0    mov $r0.11 = $r0.8   ## bblock 8, line 0,  t28,  t50
	c0    mov $r0.5 = $r0.7   ## bblock 8, line 0,  t29,  t21
	c0    goto L10?3 ## goto
	c0    NOP
;;								## 0
	c0    mov $r0.11 = $r0.8   ## bblock 10, line 0,  t28,  t50
	c0    mov $r0.5 = $r0.4   ## bblock 10, line 0,  t29,  t46
	c0    goto L10?3 ## goto
	c0    NOP
;;								## 0
	c0    NOP
	c0    mov $r0.11 = $r0.9   ## bblock 12, line 0,  t28,  t49
	c0    mov $r0.5 = $r0.4   ## bblock 12, line 0,  t29,  t46
	c0    goto L10?3 ## goto
;;								## 0
	c0    mov $r0.11 = $r0.9   ## bblock 14, line 0,  t28,  t49
	c0    NOP
	c0    mov $r0.5 = $r0.6   ## bblock 14, line 0,  t29,  t45
	c0    goto L10?3 ## goto
;;								## 0
	c0    NOP
	c0    mov $r0.11 = $r0.10   ## bblock 16, line 0,  t28,  t48
	c0    mov $r0.5 = $r0.6   ## bblock 16, line 0,  t29,  t45
	c0    goto L10?3 ## goto
;;								## 0
	c0    mov $r0.11 = $r0.10   ## bblock 18, line 0,  t28,  t48
	c0    NOP
	c0    NOP
	c0    goto L10?3 ## goto
;;								## 0
	c0    NOP
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 0
	c0    NOP
	c0    NOP
	c0    add $r0.7 = $r0.5, $r0.11   ## bblock 3, line 0,  t21,  t29,  t28
	c0    goto L0?3 ## goto
;;								## 1
	c0    NOP
	c0    and $r0.2 = $r0.3, (~0x55555555)   ## bblock 0, line 11,  t3,  t2,  (~0x55555555)(I32)
	c0    NOP
	c0    NOP
;;								## 0
	c0    and $r0.3 = $r0.3, 1431655765   ## bblock 0, line 11,  t1,  t2,  1431655765(I32)
	c0    NOP
	c0    NOP
	c0    shr $r0.2 = $r0.2, 1   ## bblock 0, line 11,  t4(SI31),  t3,  1(SI32)
;;								## 1
	c0    NOP
	c0    add $r0.3 = $r0.3, $r0.2   ## bblock 0, line 11,  t8,  t1,  t4(SI31)
	c0    NOP
	c0    NOP
;;								## 2
	c0    NOP
	c0    NOP
	c0    and $r0.3 = $r0.3, 858993459   ## bblock 0, line 12,  t7,  t8,  858993459(I32)
	c0    and $r0.2 = $r0.3, (~0x33333333)   ## bblock 0, line 12,  t9,  t8,  (~0x33333333)(I32)
;;								## 3
	c0    shr $r0.2 = $r0.2, 2   ## bblock 0, line 12,  t10(SI30),  t9,  2(SI32)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 4
	c0    NOP
	c0    NOP
	c0    add $r0.3 = $r0.3, $r0.2   ## bblock 0, line 12,  t14,  t7,  t10(SI30)
	c0    NOP
;;								## 5
	c0    NOP
	c0    NOP
	c0    and $r0.3 = $r0.3, 252645135   ## bblock 0, line 13,  t13,  t14,  252645135(I32)
	c0    and $r0.2 = $r0.3, (~0xf0f0f0f)   ## bblock 0, line 13,  t15,  t14,  (~0xf0f0f0f)(I32)
;;								## 6
	c0    shr $r0.2 = $r0.2, 4   ## bblock 0, line 13,  t16(SI28),  t15,  4(SI32)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 7
	c0    add $r0.3 = $r0.3, $r0.2   ## bblock 0, line 13,  t20,  t13,  t16(SI28)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 8
	c0    NOP
	c0    and $r0.3 = $r0.3, 16711935   ## bblock 0, line 14,  t19,  t20,  16711935(I32)
	c0    and $r0.2 = $r0.3, (~0xff00ff)   ## bblock 0, line 14,  t21,  t20,  (~0xff00ff)(I32)
	c0    NOP
;;								## 9
	c0    NOP
	c0    shr $r0.2 = $r0.2, 8   ## bblock 0, line 14,  t22(SI24),  t21,  8(SI32)
	c0    NOP
	c0    NOP
;;								## 10
	c0    NOP
	c0    NOP
	c0    add $r0.3 = $r0.3, $r0.2   ## bblock 0, line 14,  t26,  t19,  t22(SI24)
	c0    NOP
;;								## 11
	c0    and $r0.3 = $r0.3, 65535   ## bblock 0, line 15,  t25,  t26,  65535(I32)
	c0    NOP
	c0    NOP
	c0    and $r0.2 = $r0.3, (~0xffff)   ## bblock 0, line 15,  t27,  t26,  (~0xffff)(I32)
;;								## 12
	c0    NOP
	c0    NOP
	c0    NOP
	c0    shr $r0.2 = $r0.2, 16   ## bblock 0, line 15,  t28(SI16),  t27,  16(SI32)
;;								## 13
	c0    NOP
	c0    add $r0.3 = $r0.3, $r0.2   ## bblock 0, line 15,  t30,  t25,  t28(SI16)
	c0    return $r0.1 = $r0.1, (0x0), $l0.0   ## bblock 0, line 16,  t32,  ?2.1?2auto_size(I32),  t31
	c0    NOP
;;								## 14
	c0    NOP
	c0    NOP
	c0    NOP
	c0    and $r0.2 = $r0.3, (~0xfffffff)   ## bblock 0, line 52,  t2,  t36,  (~0xfffffff)(I32)
;;								## 0
	c0    shru $r0.2 = $r0.2, 28   ## bblock 0, line 52,  t3(I4),  t2,  28(SI32)
	c0    NOP
	c0    and $r0.5 = $r0.3, 983040   ## bblock 0, line 49,  t17,  t36,  983040(I32)
	c0    and $r0.4 = $r0.3, 15   ## bblock 0, line 45,  t37,  t36,  15(I32)
;;								## 1
	c0    ldb $r0.2 = (_?1PACKET.1 + 0)[$r0.2]   ## bblock 0, line 52, t4(SI8), t3(I4)
	c0    shru $r0.5 = $r0.5, 16   ## bblock 0, line 49,  t18(I16),  t17,  16(SI32)
	c0    NOP
	c0    and $r0.6 = $r0.3, 240   ## bblock 0, line 46,  t32,  t36,  240(I32)
;;								## 2
	c0    NOP
	c0    and $r0.7 = $r0.3, 61440   ## bblock 0, line 48,  t22,  t36,  61440(I32)
	c0    ldb $r0.4 = (_?1PACKET.1 + 0)[$r0.4]   ## bblock 0, line 45, t38(SI8), t37
	c0    NOP
;;								## 3
	c0    ldb $r0.5 = (_?1PACKET.1 + 0)[$r0.5]   ## bblock 0, line 49, t19(SI8), t18(I16)
	c0    shru $r0.7 = $r0.7, 12   ## bblock 0, line 48,  t23(I20),  t22,  12(SI32)
	c0    NOP
	c0    shru $r0.6 = $r0.6, 4   ## bblock 0, line 46,  t33(I28),  t32,  4(SI32)
;;								## 4
	c0    and $r0.8 = $r0.3, 251658240   ## bblock 0, line 51,  t7,  t36,  251658240(I32)
	c0    NOP
	c0    NOP
	c0    ldb $r0.7 = (_?1PACKET.1 + 0)[$r0.7]   ## bblock 0, line 48, t24(SI8), t23(I20)
;;								## 5
	c0    shru $r0.8 = $r0.8, 24   ## bblock 0, line 51,  t8(I8),  t7,  24(SI32)
	c0    ldb $r0.6 = (_?1PACKET.1 + 0)[$r0.6]   ## bblock 0, line 46, t34(SI8), t33(I28)
	c0    NOP
	c0    add $r0.2 = $r0.2, $r0.4   ## bblock 0, line 52,  t53,  t4(SI8),  t38(SI8)
;;								## 6
	c0    NOP
	c0    ldb $r0.8 = (_?1PACKET.1 + 0)[$r0.8]   ## bblock 0, line 51, t9(SI8), t8(I8)
	c0    NOP
	c0    and $r0.4 = $r0.3, 15728640   ## bblock 0, line 50,  t12,  t36,  15728640(I32)
;;								## 7
	c0    shru $r0.4 = $r0.4, 20   ## bblock 0, line 50,  t13(I12),  t12,  20(SI32)
	c0    and $r0.3 = $r0.3, 3840   ## bblock 0, line 47,  t27,  t36,  3840(I32)
	c0    NOP
	c0    add $r0.5 = $r0.5, $r0.7   ## bblock 0, line 52,  t56,  t19(SI8),  t24(SI8)
;;								## 8
	c0    ldb $r0.4 = (_?1PACKET.1 + 0)[$r0.4]   ## bblock 0, line 50, t14(SI8), t13(I12)
	c0    shru $r0.3 = $r0.3, 8   ## bblock 0, line 47,  t28(I24),  t27,  8(SI32)
	c0    add $r0.2 = $r0.2, $r0.5   ## bblock 0, line 52,  t57,  t53,  t56
	c0    NOP
;;								## 9
	c0    ldb $r0.3 = (_?1PACKET.1 + 0)[$r0.3]   ## bblock 0, line 47, t29(SI8), t28(I24)
	c0    NOP
	c0    add $r0.8 = $r0.8, $r0.6   ## bblock 0, line 52,  t54,  t9(SI8),  t34(SI8)
	c0    NOP
;;								## 12
	c0    add $r0.4 = $r0.4, $r0.3   ## bblock 0, line 52,  t55,  t14(SI8),  t29(SI8)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 13
	c0    NOP
	c0    NOP
	c0    NOP
	c0    add $r0.8 = $r0.8, $r0.4   ## bblock 0, line 52,  t58,  t54,  t55
;;								## 14
	c0    add $r0.3 = $r0.2, $r0.8   ## bblock 0, line 52,  t39,  t57,  t58
	c0    return $r0.1 = $r0.1, (0x0), $l0.0   ## bblock 0, line 52,  t41,  ?2.1?2auto_size(I32),  t40
	c0    NOP
	c0    NOP
;;								## 15
	c0    add $r0.1 = $r0.1, (-0x20)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 0
	c0    mov $r0.2 = $r0.1   ## bblock 0, line 67,  t0,  t16
	c0    stw 0[$r0.1] = $r0.3   ## bblock 0, line 69, t16, t1
	c0    NOP
	c0    NOP
;;								## 1
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldbu $r0.4 = 2[$r0.2]   ## bblock 0, line 72, t3(I8), t0
;;								## 2
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldbu $r0.5 = 3[$r0.2]   ## bblock 0, line 72, t6(I8), t0
;;								## 3
	c0    NOP
	c0    ldbu $r0.6 = 1[$r0.2]   ## bblock 0, line 71, t9(I8), t0
	c0    NOP
	c0    NOP
;;								## 4
	c0    NOP
	c0    NOP
	c0    ldb $r0.4 = (_?1PACKET.1 + 0)[$r0.4]   ## bblock 0, line 72, t4(SI8), t3(I8)
	c0    NOP
;;								## 5
	c0    ldb $r0.5 = (_?1PACKET.1 + 0)[$r0.5]   ## bblock 0, line 72, t7(SI8), t6(I8)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 6
	c0    NOP
	c0    NOP
	c0    ldb $r0.6 = (_?1PACKET.1 + 0)[$r0.6]   ## bblock 0, line 71, t10(SI8), t9(I8)
	c0    NOP
;;								## 7
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldbu $r0.2 = 0[$r0.2]   ## bblock 0, line 71, t12(I8), t0
;;								## 8
	c0    NOP
	c0    add $r0.4 = $r0.4, $r0.5   ## bblock 0, line 72,  t28,  t4(SI8),  t7(SI8)
	c0    NOP
	c0    NOP
;;								## 10
	c0    NOP
	c0    NOP
	c0    ldb $r0.2 = (_?1PACKET.1 + 0)[$r0.2]   ## bblock 0, line 71, t13(SI8), t12(I8)
	c0    NOP
;;								## 13
	c0    NOP
	c0    add $r0.6 = $r0.6, $r0.2   ## bblock 0, line 72,  t29,  t10(SI8),  t13(SI8)
	c0    NOP
	c0    NOP
;;								## 14
	c0    NOP
	c0    add $r0.3 = $r0.4, $r0.6   ## bblock 0, line 72,  t14,  t28,  t29
	c0    return $r0.1 = $r0.1, (0x20), $l0.0   ## bblock 0, line 72,  t16,  ?2.2?2auto_size(I32),  t15
	c0    NOP
;;								## 15
	c0    NOP
	c0    add $r0.1 = $r0.1, (-0x20)
	c0    NOP
	c0    NOP
;;								## 0
	c0    stw 0x1c[$r0.1] = $r0.3   ## bblock 0, line 81, t28, t38
	c0    NOP
	c0    NOP
	c0    add $r0.2 = $r0.1, 0x1c   ## bblock 0, line 83,  t2,  t28,  offset(x?1.5)(P32)
;;								## 1
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldbu $r0.4 = 3[$r0.2]   ## bblock 0, line 89, t23(I8), t2
;;								## 2
	c0    NOP
	c0    NOP
	c0    ldbu $r0.5 = 0x1c[$r0.1]   ## bblock 0, line 86, t4(I8), t28
	c0    NOP
;;								## 3
	c0    ldbu $r0.6 = 1[$r0.2]   ## bblock 0, line 87, t10(I8), t2
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 4
	c0    NOP
	c0    ldb $r0.4 = (_?1PACKET.1 + 0)[$r0.4]   ## bblock 0, line 89, t24(SI8), t23(I8)
	c0    NOP
	c0    NOP
;;								## 5
	c0    NOP
	c0    ldb $r0.5 = (_?1PACKET.1 + 0)[$r0.5]   ## bblock 0, line 86, t6, t4(I8)
	c0    NOP
	c0    NOP
;;								## 6
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldb $r0.6 = (_?1PACKET.1 + 0)[$r0.6]   ## bblock 0, line 87, t11(SI8), t10(I8)
;;								## 7
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldbu $r0.2 = 2[$r0.2]   ## bblock 0, line 88, t17(I8), t2
;;								## 8
	c0    NOP
	c0    NOP
	c0    NOP
	c0    add $r0.5 = $r0.5, $r0.4   ## bblock 0, line 89,  t41,  t6,  t24(SI8)
;;								## 10
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldb $r0.2 = (_?1PACKET.1 + 0)[$r0.2]   ## bblock 0, line 88, t18(SI8), t17(I8)
;;								## 13
	c0    NOP
	c0    NOP
	c0    NOP
	c0    add $r0.6 = $r0.6, $r0.2   ## bblock 0, line 89,  t42,  t11(SI8),  t18(SI8)
;;								## 14
	c0    add $r0.3 = $r0.5, $r0.6   ## bblock 0, line 89,  t26,  t41,  t42
	c0    NOP
	c0    return $r0.1 = $r0.1, (0x20), $l0.0   ## bblock 0, line 90,  t28,  ?2.3?2auto_size(I32),  t27
	c0    NOP
;;								## 15
	c0    add $r0.1 = $r0.1, (-0x20)
	c0    and $r0.2 = $r0.3, 15   ## bblock 0, line 40,  t2,  t4,  15(I32)
	c0    NOP
	c0    shr $r0.4 = $r0.3, 4   ## bblock 0, line 42,  t23(SI28),  t4,  4(SI32)
;;								## 0
	c0    cmpne $b0.0 = $r0.4, $r0.0   ## bblock 0, line 42,  t25(I1),  t23(SI28),  0(SI32)
	c0    NOP
	c0    stw 0x10[$r0.1] = $l0.0  ## spill ## t11
	c0    mov $r0.3 = $r0.4   ## t23(SI28)
;;								## 1
	c0    NOP
	c0    ldb $r0.5 = (_?1PACKET.1 + 0)[$r0.2]   ## bblock 0, line 40, t24, t2
	c0    NOP
	c0    NOP
;;								## 3
	c0    brf $b0.0, L0?3   ## bblock 0, line 42,  t25(I1)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 4
	c0    call $l0.0 = ntbl_bitcnt   ## bblock 2, line 43,  raddr(ntbl_bitcnt)(P32),  t23(SI28)
	c0    stw 0x14[$r0.1] = $r0.5  ## spill ## t24
	c0    NOP
	c0    NOP
;;								## 5
	c0    NOP
	c0    ldw $r0.5 = 0x14[$r0.1]  ## restore ## t24
	c0    NOP
	c0    NOP
;;								## 6
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $l0.0 = 0x10[$r0.1]  ## restore ## t11
;;								## 8
	c0    NOP
	c0    add $r0.5 = $r0.5, $r0.3   ## bblock 3, line 43,  t24,  t24,  t7
	c0    NOP
	c0    NOP
;;								## 9
	c0    NOP
	c0    NOP
	c0    NOP
	c0    mov $r0.3 = $r0.5   ## t24
;;								## 10
	c0    return $r0.1 = $r0.1, (0x20), $l0.0   ## bblock 1, line 45,  t12,  ?2.1?2auto_size(I32),  t11
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 11
	c0    NOP
	c0    NOP
	c0    mov $r0.3 = $r0.5   ## t24
	c0    ldw $l0.0 = 0x10[$r0.1]  ## restore ## t11
;;								## 2
	c0    NOP
	c0    NOP
	c0    NOP
	c0    goto L1?3 ## goto
;;								## 3
	c0    NOP
	c0    add $r0.1 = $r0.1, (-0x20)
	c0    shr $r0.2 = $r0.3, 8   ## bblock 0, line 56,  t7(SI24),  t6,  8(SI32)
	c0    NOP
;;								## 0
	c0    cmpne $b0.0 = $r0.2, $r0.0   ## bblock 0, line 56,  t26(I1),  t7(SI24),  0(SI32)
	c0    NOP
	c0    stw 0x10[$r0.1] = $l0.0  ## spill ## t13
	c0    NOP
;;								## 1
	c0    NOP
	c0    NOP
	c0    NOP
	c0    stw 0x1c[$r0.1] = $r0.3   ## bblock 0, line 52, t14, t6
;;								## 2
	c0    NOP
	c0    ldb $r0.4 = 0x1c[$r0.1]   ## bblock 0, line 54, t3(SI8), t14
	c0    NOP
	c0    mov $r0.3 = $r0.2   ## t7(SI24)
;;								## 5
	c0    NOP
	c0    and $r0.4 = $r0.4, 255   ## bblock 0, line 54,  t4,  t3(SI8),  255(I32)
	c0    stw 0x1c[$r0.1] = $r0.2   ## bblock 0, line 56, t14, t7(SI24)
	c0    NOP
;;								## 6
	c0    ldb $r0.2 = (_?1PACKET.1 + 0)[$r0.4]   ## bblock 0, line 54, t25, t4
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 8
	c0    NOP
	c0    NOP
	c0    brf $b0.0, L2?3   ## bblock 0, line 56,  t26(I1)
	c0    NOP
;;								## 9
	c0    NOP
	c0    NOP
	c0    call $l0.0 = btbl_bitcnt   ## bblock 2, line 57,  raddr(btbl_bitcnt)(P32),  t7(SI24)
	c0    stw 0x14[$r0.1] = $r0.2  ## spill ## t25
;;								## 10
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.2 = 0x14[$r0.1]  ## restore ## t25
;;								## 11
	c0    ldw $l0.0 = 0x10[$r0.1]  ## restore ## t13
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 13
	c0    NOP
	c0    NOP
	c0    add $r0.2 = $r0.2, $r0.3   ## bblock 3, line 57,  t25,  t25,  t9
	c0    NOP
;;								## 14
	c0    NOP
	c0    NOP
	c0    NOP
	c0    mov $r0.3 = $r0.2   ## t25
;;								## 15
	c0    NOP
	c0    NOP
	c0    NOP
	c0    return $r0.1 = $r0.1, (0x20), $l0.0   ## bblock 1, line 58,  t14,  ?2.2?2auto_size(I32),  t13
;;								## 16
	c0    mov $r0.3 = $r0.2   ## t25
	c0    NOP
	c0    NOP
	c0    ldw $l0.0 = 0x10[$r0.1]  ## restore ## t13
;;								## 2
	c0    NOP
	c0    NOP
	c0    NOP
	c0    goto L3?3 ## goto
;;								## 3
	c0    add $r0.1 = $r0.1, (-0x60)
	c0    mov $r0.6 = (~0x0)   ## bblock 0, line 24,  t108,  (~0x0)(I32)
	c0    NOP
	c0    mov $r0.5 = $r0.0   ## bblock 0, line 24,  t105,  0(I32)
;;								## 0
	c0    mov $r0.2 = $r0.0   ## bblock 0, line 24,  t106,  0(I32)
	c0    cmplt $b0.0 = $r0.3, 2   ## bblock 0, line 48,  t134(I1),  t2,  2(SI32)
	c0    NOP
	c0    stw 0x10[$r0.1] = $l0.0  ## spill ## t83
;;								## 1
	c0    NOP
	c0    stw 0x3c[$r0.1] = $r0.63  ## spill ## t93
	c0    NOP
	c0    NOP
;;								## 2
	c0    mov $r0.63 = 2146435071   ## bblock 0, line 24,  t107,  2146435071(I32)
	c0    stw 0x40[$r0.1] = $r0.62  ## spill ## t92
	c0    NOP
	c0    br $b0.0, L0?3   ## bblock 0, line 48,  t134(I1)
;;								## 3
	c0    NOP
	c0    NOP
	c0    stw 0x44[$r0.1] = $r0.61  ## spill ## t91
	c0    NOP
;;								## 4
	c0    NOP
	c0    stw 0x48[$r0.1] = $r0.60  ## spill ## t90
	c0    NOP
	c0    NOP
;;								## 5
	c0    NOP
	c0    NOP
	c0    stw 0x4c[$r0.1] = $r0.59  ## spill ## t89
	c0    NOP
;;								## 6
	c0    NOP
	c0    NOP
	c0    NOP
	c0    stw 0x50[$r0.1] = $r0.58  ## spill ## t88
;;								## 7
	c0    stw 0x54[$r0.1] = $r0.57  ## spill ## t87
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 8
	c0    stw 0x18[$r0.1] = $r0.6  ## spill ## t108
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 9
	c0    NOP
	c0    NOP
	c0    stw 0x1c[$r0.1] = $r0.5  ## spill ## t105
	c0    NOP
;;								## 10
	c0    NOP
	c0    NOP
	c0    NOP
	c0    stw 0x20[$r0.1] = $r0.2  ## spill ## t106
;;								## 11
	c0    ldw $r0.3 = 4[$r0.4]   ## bblock 1, line 52, t11, t97
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 13
	c0    call $l0.0 = atoi   ## bblock 1, line 52,  raddr(atoi)(P32),  t11
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 14
	c0    NOP
	c0    NOP
	c0    stw 0x58[$r0.1] = $r0.3  ## spill ## t8
	c0    NOP
;;								## 15
	c0    NOP
	c0    call $l0.0 = puts   ## bblock 2, line 54,  raddr(puts)(P32),  addr(_?1STRINGVAR.9)(P32)
	c0    NOP
	c0    mov $r0.3 = (_?1STRINGPACKET.9 + 0)   ## addr(_?1STRINGVAR.9)(P32)
;;								## 16
	c0    mov $r0.57 = (~0x6)   ## bblock 3, line 0,  t129,  (~0x6)(I32)
	c0    mov $r0.2 = $r0.0   ## bblock 3, line 56,  t104,  0(SI32)
	c0    NOP
	c0    ldw $r0.58 = 0x58[$r0.1]  ## restore ## t8
;;								## 17
	c0    NOP
	c0    stw 0x24[$r0.1] = $r0.2  ## spill ## t104
	c0    NOP
	c0    NOP
;;								## 18
	c0    NOP
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 19
	c0    cmplt $b0.0 = $r0.57, $r0.0   ## bblock 4, line 56,  t135(I1),  t129,  0(SI32)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 1
	c0    NOP
	c0    NOP
	c0    NOP
	c0    brf $b0.0, L3?3   ## bblock 4, line 56,  t135(I1)
;;								## 2
	c0    NOP
	c0    call $l0.0 = clock   ## bblock 8, line 57,  raddr(clock)(P32)
	c0    NOP
	c0    NOP
;;								## 3
	c0    mov $r0.60 = $r0.0   ## bblock 9, line 59,  t100,  0(SI32)
	c0    NOP
	c0    call $l0.0 = rand   ## bblock 9, line 59,  raddr(rand)(P32)
	c0    stw 0x14[$r0.1] = $r0.3  ## spill ## t18
;;								## 4
	c0    mov $r0.59 = $r0.3   ## bblock 10, line 59,  t99,  t19
	c0    sub $r0.2 = $r0.0, $r0.58   ## bblock 10, line 0,  t125,  0(I32),  t8
	c0    mov $r0.61 = $r0.58   ## t8
	c0    ldw $r0.62 = 0x14[$r0.1]  ## restore ## t18
;;								## 5
	c0    NOP
	c0    NOP
	c0    NOP
	c0    mov $r0.58 = $r0.2   ## bblock 10, line 0,  t124,  t125
;;								## 6
	c0    NOP
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 7
	c0    cmplt $b0.0 = $r0.58, $r0.0   ## bblock 11, line 59,  t138(I1),  t124,  0(SI32)
	c0    sh2add $r0.2 = $r0.57, ((_?1PACKET.13 + 0) + 28)   ## [spec] bblock 19, line 60,  t146,  t129,  (addr(pBitCntFunc?1.2)(P32) + 0x1c(I32))(P32)
	c0    mov $r0.3 = $r0.59   ## t99
	c0    NOP
;;								## 0
	c0    NOP
	c0    ldw.d $l0.0 = 0[$r0.2]   ## [spec] bblock 19, line 60, t28, t146
	c0    NOP
	c0    NOP
;;								## 1
	c0    NOP
	c0    NOP
	c0    brf $b0.0, L5?3   ## bblock 11, line 59,  t138(I1)
	c0    NOP
;;								## 2
	c0    NOP
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 4
	c0    NOP
	c0    call $l0.0 = $l0.0   ## bblock 19, line 60,  t28,  t99
	c0    NOP
	c0    NOP
;;								## 5
	c0    add $r0.60 = $r0.3, $r0.60   ## bblock 20, line 60,  t100,  t29,  t100
	c0    add $r0.58 = $r0.58, 1   ## bblock 20, line 0,  t124,  t124,  1(I32)
	c0    add $r0.59 = $r0.59, 13   ## bblock 20, line 59,  t99,  t99,  13(SI32)
	c0    goto L4?3 ## goto
;;								## 6
	c0    NOP
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 0
	c0    NOP
	c0    call $l0.0 = clock   ## bblock 15, line 62,  raddr(clock)(P32)
	c0    NOP
	c0    NOP
;;								## 1
	c0    sub $r0.3 = $r0.3, $r0.62   ## bblock 13, line 63,  t35,  t32,  t18
	c0    NOP
	c0    call $l0.0 = _d_ufloat   ## bblock 13, line 63,  raddr(_d_ufloat)(P32),  t35
	c0    NOP
;;								## 2
	c0    call $l0.0 = _d_div   ## bblock 13, line 63,  raddr(_d_div)(P32),  t65,  t66,  1093567616(I32),  0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1093567616   ## 1093567616(I32)
	c0    NOP
;;								## 3
	c0    mov $r0.5 = $r0.63   ## t107
	c0    NOP
	c0    stw 0x30[$r0.1] = $r0.3  ## spill ## t121
	c0    NOP
;;								## 4
	c0    NOP
	c0    NOP
	c0    stw 0x34[$r0.1] = $r0.4  ## spill ## t120
	c0    NOP
;;								## 5
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.6 = 0x18[$r0.1]  ## restore ## t108
;;								## 7
	c0    call $l0.0 = _d_lt   ## bblock 13, line 64,  raddr(_d_lt)(P32),  t121,  t120,  t107,  t108
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 8
	c0    cmpne $b0.0 = $r0.3, $r0.0   ## bblock 13, line 63,  t139(I1),  t114,  0(I32)
	c0    cmpne $b0.1 = $r0.3, $r0.0   ## bblock 13, line 63,  t140(I1),  t114,  0(I32)
	c0    NOP
	c0    stw 0x38[$r0.1] = $r0.3  ## spill ## t114
;;								## 9
	c0    ldw $r0.2 = 0x18[$r0.1]  ## restore ## t108
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 10
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0x34[$r0.1]  ## restore ## t120
;;								## 11
	c0    NOP
	c0    NOP
	c0    ldw $r0.3 = 0x30[$r0.1]  ## restore ## t121
	c0    NOP
;;								## 12
	c0    NOP
	c0    ldw $r0.5 = 0x1c[$r0.1]  ## restore ## t105
	c0    NOP
	c0    NOP
;;								## 13
	c0    slct $r0.58 = $b0.0, $r0.4, $r0.2   ## bblock 13, line 63,  t108,  t139(I1),  t120,  t108
	c0    ldw $r0.6 = 0x20[$r0.1]  ## restore ## t106
	c0    NOP
	c0    NOP
;;								## 14
	c0    NOP
	c0    NOP
	c0    slct $r0.63 = $b0.1, $r0.3, $r0.63   ## bblock 13, line 63,  t107,  t140(I1),  t121,  t107
	c0    stw 0x18[$r0.1] = $r0.58  ## spill ## t108
;;								## 15
	c0    NOP
	c0    call $l0.0 = _d_gt   ## bblock 13, line 68,  raddr(_d_gt)(P32),  t121,  t120,  t105,  t106
	c0    NOP
	c0    NOP
;;								## 16
	c0    cmpne $b0.0 = $r0.3, $r0.0   ## bblock 13, line 68,  t141(I1),  t118,  0(I32)
	c0    sh2add $r0.2 = $r0.57, ((_?1PACKET.14 + 0) + 28)   ## bblock 13, line 73,  t144,  t129,  (addr(text?1.2)(P32) + 0x1c(I32))(P32)
	c0    NOP
	c0    ldw $r0.8 = 0x20[$r0.1]  ## restore ## t106
;;								## 17
	c0    cmpne $b0.1 = $r0.3, $r0.0   ## bblock 13, line 68,  t142(I1),  t118,  0(I32)
	c0    ldw $r0.4 = 0[$r0.2]   ## bblock 13, line 73, t50, t144
	c0    cmpne $b0.2 = $r0.3, $r0.0   ## bblock 13, line 70,  t145(I1),  t118,  0(I32)
	c0    mov $r0.7 = $r0.60   ## t100
;;								## 18
	c0    NOP
	c0    mov $r0.3 = (_?1STRINGPACKET.10 + 0)   ## addr(_?1STRINGVAR.10)(P32)
	c0    NOP
	c0    ldw $r0.6 = 0x34[$r0.1]  ## restore ## t120
;;								## 19
	c0    NOP
	c0    NOP
	c0    ldw $r0.2 = 0x1c[$r0.1]  ## restore ## t105
	c0    NOP
;;								## 20
	c0    NOP
	c0    ldw $r0.5 = 0x30[$r0.1]  ## restore ## t121
	c0    NOP
	c0    NOP
;;								## 21
	c0    NOP
	c0    NOP
	c0    slct $r0.58 = $b0.0, $r0.6, $r0.8   ## bblock 13, line 68,  t106,  t141(I1),  t120,  t106
	c0    ldw $r0.8 = 0x38[$r0.1]  ## restore ## t114
;;								## 22
	c0    NOP
	c0    NOP
	c0    NOP
	c0    stw 0x20[$r0.1] = $r0.58  ## spill ## t106
;;								## 23
	c0    NOP
	c0    slct $r0.58 = $b0.1, $r0.5, $r0.2   ## bblock 13, line 68,  t105,  t142(I1),  t121,  t105
	c0    ldw $r0.2 = 0x24[$r0.1]  ## restore ## t104
	c0    NOP
;;								## 24
	c0    cmpne $b0.0 = $r0.8, $r0.0   ## bblock 13, line 66,  t143(I1),  t114,  0(I32)
	c0    NOP
	c0    NOP
	c0    stw 0x1c[$r0.1] = $r0.58  ## spill ## t105
;;								## 25
	c0    ldw $r0.8 = 0x28[$r0.1]  ## restore ## t103
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 26
	c0    NOP
	c0    ldw $r0.9 = 0x2c[$r0.1]  ## restore ## t102
	c0    NOP
	c0    NOP
;;								## 28
	c0    NOP
	c0    NOP
	c0    NOP
	c0    slct $r0.8 = $b0.0, $r0.2, $r0.8   ## bblock 13, line 66,  t103,  t143(I1),  t104,  t103
;;								## 29
	c0    slct $r0.9 = $b0.2, $r0.2, $r0.9   ## bblock 13, line 70,  t102,  t145(I1),  t104,  t102
	c0    NOP
	c0    stw 0x28[$r0.1] = $r0.8  ## spill ## t103
	c0    NOP
;;								## 30
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 13, line 73,  raddr(printf)(P32),  addr(_?1STRINGVAR.10)(P32),  t50,  t121,  t120,  t100
	c0    NOP
	c0    stw 0x2c[$r0.1] = $r0.9  ## spill ## t102
;;								## 31
	c0    add $r0.57 = $r0.57, 1   ## bblock 16, line 0,  t129,  t129,  1(I32)
	c0    NOP
	c0    mov $r0.58 = $r0.61   ## t8
	c0    ldw $r0.2 = 0x24[$r0.1]  ## restore ## t104
;;								## 34
	c0    add $r0.2 = $r0.2, 1   ## bblock 16, line 56,  t104,  t104,  1(SI32)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 35
	c0    stw 0x24[$r0.1] = $r0.2  ## spill ## t104
	c0    goto L2?3 ## goto
	c0    NOP
	c0    NOP
;;								## 36
	c0    mov $r0.3 = (_?1STRINGPACKET.11 + 0)   ## addr(_?1STRINGVAR.11)(P32)
	c0    NOP
	c0    ldw $r0.8 = 0x28[$r0.1]  ## restore ## t103
	c0    NOP
;;								## 2
	c0    NOP
	c0    NOP
	c0    sh2add $r0.8 = $r0.8, (_?1PACKET.14 + 0)   ## bblock 5, line 75,  t136,  t103,  addr(text?1.2)(P32)
	c0    NOP
;;								## 3
	c0    NOP
	c0    ldw $r0.4 = 0[$r0.8]   ## bblock 5, line 75, t57, t136
	c0    NOP
	c0    NOP
;;								## 5
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 5, line 75,  raddr(printf)(P32),  addr(_?1STRINGVAR.11)(P32),  t57
	c0    NOP
	c0    NOP
;;								## 6
	c0    mov $r0.3 = (_?1STRINGPACKET.12 + 0)   ## addr(_?1STRINGVAR.12)(P32)
	c0    NOP
	c0    ldw $r0.9 = 0x2c[$r0.1]  ## restore ## t102
	c0    NOP
;;								## 9
	c0    sh2add $r0.9 = $r0.9, (_?1PACKET.14 + 0)   ## bblock 6, line 76,  t137,  t102,  addr(text?1.2)(P32)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 10
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0[$r0.9]   ## bblock 6, line 76, t62, t137
	c0    NOP
;;								## 12
	c0    NOP
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 6, line 76,  raddr(printf)(P32),  addr(_?1STRINGVAR.12)(P32),  t62
	c0    NOP
;;								## 13
	c0    mov $r0.3 = $r0.0   ## 0(SI32)
	c0    ldw $l0.0 = 0x10[$r0.1]  ## restore ## t83
	c0    NOP
	c0    NOP
;;								## 14
	c0    NOP
	c0    NOP
	c0    ldw $r0.63 = 0x3c[$r0.1]  ## restore ## t93
	c0    NOP
;;								## 15
	c0    NOP
	c0    ldw $r0.62 = 0x40[$r0.1]  ## restore ## t92
	c0    NOP
	c0    NOP
;;								## 16
	c0    ldw $r0.61 = 0x44[$r0.1]  ## restore ## t91
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 17
	c0    NOP
	c0    NOP
	c0    ldw $r0.60 = 0x48[$r0.1]  ## restore ## t90
	c0    NOP
;;								## 18
	c0    NOP
	c0    NOP
	c0    ldw $r0.59 = 0x4c[$r0.1]  ## restore ## t89
	c0    NOP
;;								## 19
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.58 = 0x50[$r0.1]  ## restore ## t88
;;								## 20
	c0    NOP
	c0    ldw $r0.57 = 0x54[$r0.1]  ## restore ## t87
	c0    NOP
	c0    NOP
;;								## 22
	c0    NOP
	c0    return $r0.1 = $r0.1, (0x60), $l0.0   ## bblock 7, line 77,  t84,  ?2.1?2auto_size(I32),  t83
	c0    NOP
	c0    NOP
;;								## 23
	c0    NOP
	c0    NOP
	c0    NOP
	c0    stw 0x5c[$r0.1] = $r0.4  ## spill ## t97
;;								## 0
	c0    stw 0x20[$r0.1] = $r0.2  ## spill ## t106
	c0    NOP
	c0    mov $r0.4 = (_?1STRINGPACKET.8 + 0)   ## addr(_?1STRINGVAR.8)(P32)
	c0    NOP
;;								## 1
	c0    NOP
	c0    NOP
	c0    stw 0x1c[$r0.1] = $r0.5  ## spill ## t105
	c0    NOP
;;								## 2
	c0    stw 0x18[$r0.1] = $r0.6  ## spill ## t108
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 3
	c0    NOP
	c0    NOP
	c0    ldw $r0.7 = ((_impure_ptr + 0) + 0)[$r0.0]   ## bblock 21, line 49, t4, 0(I32)
	c0    NOP
;;								## 6
	c0    NOP
	c0    NOP
	c0    ldw $r0.3 = 12[$r0.7]   ## bblock 21, line 49, t5, t4
	c0    NOP
;;								## 8
	c0    NOP
	c0    NOP
	c0    call $l0.0 = fprintf   ## bblock 21, line 49,  raddr(fprintf)(P32),  t5,  addr(_?1STRINGVAR.8)(P32)
	c0    NOP
;;								## 9
	c0    NOP
	c0    NOP
	c0    call $l0.0 = exit   ## bblock 22, line 50,  raddr(exit)(P32),  -1(SI32)
	c0    mov $r0.3 = -1   ## -1(SI32)
;;								## 10
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0x5c[$r0.1]  ## restore ## t97
	c0    NOP
;;								## 11
	c0    ldw $r0.2 = 0x20[$r0.1]  ## restore ## t106
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 12
	c0    NOP
	c0    ldw $r0.5 = 0x1c[$r0.1]  ## restore ## t105
	c0    NOP
	c0    NOP
;;								## 13
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.6 = 0x18[$r0.1]  ## restore ## t108
;;								## 15
	c0    NOP
	c0    goto L1?3 ## goto
	c0    NOP
	c0    NOP
;;								## 16
	c0    NOP
	c0    mov $r0.16 = 7   ## bblock 0, line 0,  t70,  7(I32)
	c0    mov $r0.15 = 6   ## bblock 0, line 0,  t69,  6(I32)
	c0    NOP
;;								## 0
	c0    mov $r0.14 = 5   ## bblock 0, line 0,  t68,  5(I32)
	c0    mov $r0.13 = 4   ## bblock 0, line 0,  t67,  4(I32)
	c0    mov $r0.12 = 3   ## bblock 0, line 0,  t66,  3(I32)
	c0    mov $r0.11 = 2   ## bblock 0, line 0,  t65,  2(I32)
;;								## 1
	c0    mov $r0.6 = $r0.0   ## bblock 0, line 84,  t34,  0(SI32)
	c0    mov $r0.2 = $r0.0   ## bblock 0, line 84,  t26,  0(SI32)
	c0    mov $r0.10 = $r0.0   ## bblock 0, line 84,  t27,  0(SI32)
	c0    mov $r0.8 = 1   ## bblock 0, line 0,  t71,  1(I32)
;;								## 2
	c0    NOP
	c0    NOP
	c0    NOP
	c0    mov $r0.4 = $r0.3   ## t24(SI31)
;;								## 3
	c0    cmpltu $r0.3 = $r0.2, 32   ## bblock 1, line 84,  t2,  t26,  32(I32)
	c0    and $r0.5 = $r0.4, 1   ## [spec] bblock 3, line 85,  t9,  t24(SI31),  1(I32)
	c0    shr $r0.7 = $r0.4, 1   ## [spec] bblock 3, line 84,  t29(SI31),  t24(SI31),  1(SI32)
	c0    cmpltu $r0.9 = $r0.8, 32   ## [spec] bblock 3, line 84-1,  t60,  t71,  32(I32)
;;								## 0
	c0    andl $b0.0 = $r0.4, $r0.3   ## bblock 1, line 84,  t93(I1),  t24(SI31),  t2
	c0    add $r0.3 = $r0.5, $r0.6   ## [spec] bblock 3, line 85,  t32,  t9,  t34
	c0    andl $b0.1 = $r0.7, $r0.9   ## [spec] bblock 3, line 84-1,  t94(I1),  t29(SI31),  t60
	c0    and $r0.9 = $r0.7, 1   ## [spec] bblock 23, line 85-1,  t61,  t29(SI31),  1(I32)
;;								## 1
	c0    shr $r0.7 = $r0.7, 1   ## [spec] bblock 23, line 84-1,  t62(SI31),  t29(SI31),  1(SI32)
	c0    add $r0.17 = $r0.9, $r0.10   ## [spec] bblock 23, line 85-1,  t64,  t61,  t27
	c0    cmpltu $r0.18 = $r0.11, 32   ## [spec] bblock 23, line 84-2,  t55,  t65,  32(I32)
	c0    cmpltu $r0.19 = $r0.12, 32   ## [spec] bblock 20, line 84-3,  t50,  t66,  32(I32)
;;								## 2
	c0    andl $b0.0 = $r0.7, $r0.18   ## [spec] bblock 23, line 84-2,  t112(I1),  t62(SI31),  t55
	c0    and $r0.18 = $r0.7, 1   ## [spec] bblock 20, line 85-2,  t56,  t62(SI31),  1(I32)
	c0    shr $r0.7 = $r0.7, 1   ## [spec] bblock 20, line 84-2,  t57(SI31),  t62(SI31),  1(SI32)
	c0    brf $b0.0, L7?3   ## bblock 1, line 84,  t93(I1)
;;								## 3
	c0    add $r0.20 = $r0.5, $r0.18   ## [spec] bblock 20, line 85-2,  t110,  t9,  t56
	c0    andl $b0.1 = $r0.7, $r0.19   ## [spec] bblock 20, line 84-3,  t111(I1),  t57(SI31),  t50
	c0    and $r0.19 = $r0.7, 1   ## [spec] bblock 17, line 85-3,  t51,  t57(SI31),  1(I32)
	c0    brf $b0.1, L8?3   ## bblock 3, line 84-1,  t94(I1)
;;								## 4
	c0    add $r0.20 = $r0.20, $r0.6   ## [spec] bblock 20, line 85-2,  t59,  t110,  t34
	c0    shr $r0.7 = $r0.7, 1   ## [spec] bblock 17, line 84-3,  t52(SI31),  t57(SI31),  1(SI32)
	c0    add $r0.21 = $r0.9, $r0.19   ## [spec] bblock 17, line 85-3,  t108,  t61,  t51
	c0    brf $b0.0, L9?3   ## bblock 23, line 84-2,  t112(I1)
;;								## 5
	c0    add $r0.21 = $r0.21, $r0.10   ## [spec] bblock 17, line 85-3,  t54,  t108,  t27
	c0    cmpltu $r0.3 = $r0.13, 32   ## [spec] bblock 17, line 84-4,  t45,  t67,  32(I32)
	c0    and $r0.22 = $r0.7, 1   ## [spec] bblock 14, line 85-4,  t46,  t52(SI31),  1(I32)
	c0    brf $b0.1, L10?3   ## bblock 20, line 84-3,  t111(I1)
;;								## 6
	c0    andl $b0.0 = $r0.7, $r0.3   ## bblock 17, line 84-4,  t109(I1),  t52(SI31),  t45
	c0    shr $r0.7 = $r0.7, 1   ## [spec] bblock 14, line 84-4,  t47(SI31),  t52(SI31),  1(SI32)
	c0    add $r0.3 = $r0.5, $r0.22   ## [spec] bblock 14, line 85-4,  t105,  t9,  t46
	c0    add $r0.17 = $r0.6, $r0.18   ## [spec] bblock 14, line 85-4,  t106,  t34,  t56
;;								## 7
	c0    add $r0.3 = $r0.3, $r0.17   ## [spec] bblock 14, line 85-4,  t49,  t105,  t106
	c0    cmpltu $r0.17 = $r0.14, 32   ## [spec] bblock 14, line 84-5,  t40,  t68,  32(I32)
	c0    and $r0.23 = $r0.7, 1   ## [spec] bblock 11, line 85-5,  t41,  t47(SI31),  1(I32)
	c0    add $r0.24 = $r0.10, $r0.19   ## [spec] bblock 11, line 85-5,  t103,  t27,  t51
;;								## 8
	c0    andl $b0.0 = $r0.7, $r0.17   ## [spec] bblock 14, line 84-5,  t107(I1),  t47(SI31),  t40
	c0    shr $r0.7 = $r0.7, 1   ## [spec] bblock 11, line 84-5,  t42(SI31),  t47(SI31),  1(SI32)
	c0    add $r0.17 = $r0.9, $r0.23   ## [spec] bblock 11, line 85-5,  t102,  t61,  t41
	c0    brf $b0.0, L11?3   ## bblock 17, line 84-4,  t109(I1)
;;								## 9
	c0    add $r0.17 = $r0.17, $r0.24   ## [spec] bblock 11, line 85-5,  t44,  t102,  t103
	c0    cmpltu $r0.22 = $r0.15, 32   ## [spec] bblock 11, line 84-6,  t36,  t69,  32(I32)
	c0    and $r0.20 = $r0.7, 1   ## [spec] bblock 8, line 85-6,  t37,  t42(SI31),  1(I32)
	c0    add $r0.6 = $r0.6, $r0.22   ## [spec] bblock 8, line 85-6,  t99,  t34,  t46
;;								## 10
	c0    andl $b0.0 = $r0.7, $r0.22   ## [spec] bblock 11, line 84-6,  t104(I1),  t42(SI31),  t36
	c0    shr $r0.7 = $r0.7, 1   ## [spec] bblock 8, line 84-6,  t38(SI31),  t42(SI31),  1(SI32)
	c0    add $r0.5 = $r0.5, $r0.20   ## [spec] bblock 8, line 85-6,  t98,  t9,  t37
	c0    brf $b0.0, L12?3   ## bblock 14, line 84-5,  t107(I1)
;;								## 11
	c0    cmpltu $r0.18 = $r0.16, 32   ## [spec] bblock 8, line 84-7,  t31,  t70,  32(I32)
	c0    add $r0.5 = $r0.5, $r0.18   ## [spec] bblock 8, line 85-6,  t100,  t98,  t56
	c0    and $r0.20 = $r0.7, 1   ## [spec] bblock 5, line 85-7,  t30,  t38(SI31),  1(I32)
	c0    add $r0.10 = $r0.10, $r0.23   ## [spec] bblock 5, line 85-7,  t96,  t27,  t41
;;								## 12
	c0    add $r0.6 = $r0.5, $r0.6   ## [spec] bblock 8, line 85-6,  t34,  t100,  t99
	c0    andl $b0.0 = $r0.7, $r0.18   ## [spec] bblock 8, line 84-7,  t101(I1),  t38(SI31),  t31
	c0    add $r0.9 = $r0.9, $r0.20   ## [spec] bblock 5, line 85-7,  t95,  t61,  t30
	c0    brf $b0.0, L13?3   ## bblock 11, line 84-6,  t104(I1)
;;								## 13
	c0    shr $r0.4 = $r0.7, 1   ## [spec] bblock 5, line 84-7,  t24(SI31),  t38(SI31),  1(SI32)
	c0    add $r0.11 = $r0.11, 8   ## [spec] bblock 5, line 0,  t65,  t65,  8(I32)
	c0    add $r0.2 = $r0.2, 8   ## [spec] bblock 5, line 84-7,  t26,  t26,  8(SI32)
	c0    add $r0.9 = $r0.9, $r0.19   ## [spec] bblock 5, line 85-7,  t97,  t95,  t51
;;								## 14
	c0    add $r0.13 = $r0.13, 8   ## [spec] bblock 5, line 0,  t67,  t67,  8(I32)
	c0    add $r0.12 = $r0.12, 8   ## [spec] bblock 5, line 0,  t66,  t66,  8(I32)
	c0    add $r0.10 = $r0.9, $r0.10   ## [spec] bblock 5, line 85-7,  t27,  t97,  t96
	c0    brf $b0.0, L14?3   ## bblock 8, line 84-7,  t101(I1)
;;								## 15
	c0    add $r0.8 = $r0.8, 8   ## bblock 5, line 0,  t71,  t71,  8(I32)
	c0    add $r0.16 = $r0.16, 8   ## bblock 5, line 0,  t70,  t70,  8(I32)
	c0    add $r0.15 = $r0.15, 8   ## bblock 5, line 0,  t69,  t69,  8(I32)
	c0    add $r0.14 = $r0.14, 8   ## bblock 5, line 0,  t68,  t68,  8(I32)
;;
	c0    NOP
	c0    NOP
	c0    goto L6?3 ## goto
	c0    NOP
;;								## 16
	c0    NOP
	c0    mov $r0.10 = $r0.17   ## bblock 6, line 0,  t33,  t44
	c0    goto L15?3 ## goto
	c0    NOP
;;								## 0
	c0    NOP
	c0    mov $r0.10 = $r0.17   ## bblock 9, line 0,  t33,  t44
	c0    mov $r0.6 = $r0.3   ## bblock 9, line 0,  t35,  t49
	c0    goto L15?3 ## goto
;;								## 0
	c0    mov $r0.10 = $r0.21   ## bblock 12, line 0,  t33,  t54
	c0    mov $r0.6 = $r0.3   ## bblock 12, line 0,  t35,  t49
	c0    goto L15?3 ## goto
	c0    NOP
;;								## 0
	c0    mov $r0.10 = $r0.21   ## bblock 15, line 0,  t33,  t54
	c0    NOP
	c0    mov $r0.6 = $r0.20   ## bblock 15, line 0,  t35,  t59
	c0    goto L15?3 ## goto
;;								## 0
	c0    mov $r0.10 = $r0.17   ## bblock 18, line 0,  t33,  t64
	c0    mov $r0.6 = $r0.20   ## bblock 18, line 0,  t35,  t59
	c0    goto L15?3 ## goto
	c0    NOP
;;								## 0
	c0    mov $r0.10 = $r0.17   ## bblock 21, line 0,  t33,  t64
	c0    mov $r0.6 = $r0.3   ## bblock 21, line 0,  t35,  t32
	c0    NOP
	c0    goto L15?3 ## goto
;;								## 0
	c0    mov $r0.6 = $r0.3   ## bblock 24, line 0,  t35,  t32
	c0    NOP
	c0    NOP
	c0    goto L15?3 ## goto
;;								## 0
	c0    NOP
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 0
	c0    add $r0.3 = $r0.6, $r0.10   ## bblock 2, line 0,  t25,  t35,  t33
	c0    return $r0.1 = $r0.1, (0x0), $l0.0   ## bblock 2, line 86,  t13,  ?2.2?2auto_size(I32),  t12
	c0    NOP
	c0    NOP
;;								## 1
	c0    NOP
	c0    NOP
	c0    add $r0.1 = $r0.1, (-0x20)
	c0    NOP
;;								## 0
	c0    stw 0x10[$r0.1] = $l0.0  ## spill ## t12
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 1
	c0    NOP
	c0    stw 0x14[$r0.1] = $r0.4  ## spill ## t26
	c0    NOP
	c0    NOP
;;								## 2
	c0    stw 0x18[$r0.1] = $r0.3  ## spill ## t25
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 3
	c0    NOP
	c0    call $l0.0 = malloc   ## bblock 0, line 16,  raddr(malloc)(P32),  8(I32)
	c0    NOP
	c0    mov $r0.3 = 8   ## 8(I32)
;;								## 4
	c0    cmpeq $b0.0 = $r0.3, $r0.0   ## bblock 1, line 17,  t28(I1),  t0,  0x0(P32)
	c0    mov $r0.3 = $r0.0   ## 0x0(P32)
	c0    ldw $l0.0 = 0x10[$r0.1]  ## restore ## t12
	c0    mov $r0.2 = $r0.3   ## t0
;;								## 6
	c0    NOP
	c0    NOP
	c0    brf $b0.0, L0?3   ## bblock 1, line 17,  t28(I1)
	c0    NOP
;;								## 7
	c0    NOP
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 8
	c0    return $r0.1 = $r0.1, (0x20), $l0.0   ## bblock 7, line 18,  t13,  ?2.1?2auto_size(I32),  t12
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 9
	c0    NOP
	c0    NOP
	c0    NOP
	c0    stw 0x1c[$r0.1] = $r0.2  ## spill ## t0
;;								## 0
	c0    ldw $r0.4 = 0x14[$r0.1]  ## restore ## t26
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 1
	c0    ldw $r0.3 = 0x18[$r0.1]  ## restore ## t25
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 3
	c0    NOP
	c0    NOP
	c0    NOP
	c0    call $l0.0 = fopen   ## bblock 2, line 19,  raddr(fopen)(P32),  t25,  t26
;;								## 4
	c0    NOP
	c0    cmpeq $b0.0 = $r0.3, $r0.0   ## bblock 3, line 20,  t29(I1),  t2,  0x0(P32)
	c0    ldw $r0.2 = 0x1c[$r0.1]  ## restore ## t0
	c0    NOP
;;								## 7
	c0    stw 0[$r0.2] = $r0.3   ## bblock 3, line 19, t0, t2
	c0    NOP
	c0    NOP
	c0    brf $b0.0, L1?3   ## bblock 3, line 20,  t29(I1)
;;								## 8
	c0    NOP
	c0    NOP
	c0    call $l0.0 = free   ## bblock 5, line 22,  raddr(free)(P32),  t0
	c0    mov $r0.3 = $r0.2   ## t0
;;								## 9
	c0    ldw $l0.0 = 0x10[$r0.1]  ## restore ## t12
	c0    NOP
	c0    NOP
	c0    mov $r0.3 = $r0.0   ## 0x0(P32)
;;								## 13
	c0    NOP
	c0    NOP
	c0    NOP
	c0    return $r0.1 = $r0.1, (0x20), $l0.0   ## bblock 6, line 23,  t13,  ?2.1?2auto_size(I32),  t12
;;								## 14
	c0    ldw $l0.0 = 0x10[$r0.1]  ## restore ## t12
	c0    mov $r0.3 = $r0.2   ## t0
	c0    NOP
	c0    NOP
;;								## 0
	c0    NOP
	c0    stb 5[$r0.2] = $r0.0   ## bblock 4, line 25, t0, 0(SI32)
	c0    NOP
	c0    NOP
;;								## 1
	c0    NOP
	c0    stb 7[$r0.2] = $r0.0   ## bblock 4, line 26, t0, 0(SI32)
	c0    NOP
	c0    NOP
;;								## 3
	c0    NOP
	c0    return $r0.1 = $r0.1, (0x20), $l0.0   ## bblock 4, line 27,  t13,  ?2.1?2auto_size(I32),  t12
	c0    NOP
	c0    NOP
;;								## 4
	c0    add $r0.1 = $r0.1, (-0x20)
	c0    NOP
	c0    ldb $r0.2 = 5[$r0.3]   ## bblock 0, line 32, t1(SI8), t31
	c0    NOP
;;								## 0
	c0    stw 0x10[$r0.1] = $l0.0  ## spill ## t19
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 1
	c0    stw 0x14[$r0.1] = $r0.3  ## spill ## t31
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 2
	c0    cmpeq $b0.0 = $r0.2, $r0.0   ## bblock 0, line 32,  t32(I1),  t1(SI8),  0(SI32)
	c0    NOP
	c0    ldw.d $r0.3 = 0[$r0.3]   ## [spec] bblock 2, line 34, t4, t31
	c0    NOP
;;								## 4
	c0    NOP
	c0    brf $b0.0, L2?3   ## bblock 0, line 32,  t32(I1)
	c0    NOP
	c0    NOP
;;								## 5
	c0    NOP
	c0    call $l0.0 = fgetc   ## bblock 2, line 34,  raddr(fgetc)(P32),  t4
	c0    NOP
	c0    NOP
;;								## 6
	c0    mov $r0.5 = 1   ## 1(SI32)
	c0    mov $r0.4 = 8   ## 8(SI32)
	c0    NOP
	c0    ldw $r0.2 = 0x14[$r0.1]  ## restore ## t31
;;								## 7
	c0    ldw $l0.0 = 0x10[$r0.1]  ## restore ## t19
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 9
	c0    NOP
	c0    stb 5[$r0.2] = $r0.4   ## bblock 3, line 35, t31, 8(SI32)
	c0    NOP
	c0    NOP
;;								## 10
	c0    NOP
	c0    NOP
	c0    ldb $r0.4 = 5[$r0.2]   ## bblock 1, line 37, t9(SI8), t31
	c0    NOP
;;								## 11
	c0    NOP
	c0    NOP
	c0    stb 4[$r0.2] = $r0.3   ## bblock 3, line 34, t31, t2
	c0    NOP
;;								## 12
	c0    ldb $r0.6 = 4[$r0.2]   ## bblock 1, line 38, t16(SI8), t31
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 13
	c0    NOP
	c0    add $r0.4 = $r0.4, -1   ## bblock 1, line 37,  t10,  t9(SI8),  -1(SI32)
	c0    NOP
	c0    NOP
;;								## 14
	c0    sxtb $r0.7 = $r0.4   ## bblock 1, line 37,  t13(SI8),  t10
	c0    NOP
	c0    stb 5[$r0.2] = $r0.4   ## bblock 1, line 37, t31, t10
	c0    NOP
;;								## 15
	c0    NOP
	c0    NOP
	c0    shl $r0.5 = $r0.5, $r0.7   ## bblock 1, line 38,  t14,  1(SI32),  t13(SI8)
	c0    NOP
;;								## 16
	c0    NOP
	c0    NOP
	c0    and $r0.5 = $r0.5, $r0.6   ## bblock 1, line 38,  t17,  t14,  t16(SI8)
	c0    NOP
;;								## 17
	c0    cmpne $r0.3 = $r0.5, $r0.0   ## bblock 1, line 38,  t18,  t17,  0(I32)
	c0    return $r0.1 = $r0.1, (0x20), $l0.0   ## bblock 1, line 38,  t20,  ?2.2?2auto_size(I32),  t19
	c0    NOP
	c0    NOP
;;								## 18
	c0    NOP
	c0    ldw $l0.0 = 0x10[$r0.1]  ## restore ## t19
	c0    NOP
	c0    mov $r0.5 = 1   ## 1(SI32)
;;								## 0
	c0    ldw $r0.2 = 0x14[$r0.1]  ## restore ## t31
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 3
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldb $r0.4 = 5[$r0.2]   ## bblock 1, line 37, t9(SI8), t31
;;								## 4
	c0    NOP
	c0    NOP
	c0    NOP
	c0    goto L3?3 ## goto
;;								## 5
	c0    add $r0.1 = $r0.1, (-0x20)
	c0    NOP
	c0    NOP
	c0    ldb $r0.2 = 7[$r0.4]   ## bblock 0, line 43, t1(SI8), t36
;;								## 0
	c0    NOP
	c0    NOP
	c0    stw 0x10[$r0.1] = $l0.0  ## spill ## t22
	c0    NOP
;;								## 1
	c0    NOP
	c0    stw 0x14[$r0.1] = $r0.3  ## spill ## t35
	c0    NOP
	c0    NOP
;;								## 2
	c0    NOP
	c0    cmpeq $b0.0 = $r0.2, 8   ## bblock 0, line 43,  t37(I1),  t1(SI8),  8(SI32)
	c0    stw 0x18[$r0.1] = $r0.4  ## spill ## t36
	c0    NOP
;;								## 3
	c0    NOP
	c0    NOP
	c0    ldb.d $r0.3 = 6[$r0.4]   ## [spec] bblock 2, line 45, t4(SI8), t36
	c0    NOP
;;								## 4
	c0    ldw.d $r0.2 = 0[$r0.4]   ## [spec] bblock 2, line 45, t6, t36
	c0    NOP
	c0    brf $b0.0, L4?3   ## bblock 0, line 43,  t37(I1)
	c0    NOP
;;								## 5
	c0    NOP
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 7
	c0    NOP
	c0    NOP
	c0    call $l0.0 = fputc   ## bblock 2, line 45,  raddr(fputc)(P32),  t4(SI8),  t6
	c0    mov $r0.4 = $r0.2   ## [spec] t6
;;								## 8
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0x18[$r0.1]  ## restore ## t36
;;								## 9
	c0    NOP
	c0    NOP
	c0    ldw $r0.3 = 0x14[$r0.1]  ## restore ## t35
	c0    NOP
;;								## 10
	c0    ldw $l0.0 = 0x10[$r0.1]  ## restore ## t22
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 11
	c0    NOP
	c0    NOP
	c0    ldb $r0.2 = 6[$r0.4]   ## bblock 1, line 49, t13(SI8), t36
	c0    NOP
;;								## 12
	c0    stb 7[$r0.4] = $r0.0   ## bblock 3, line 46, t36, 0(SI32)
	c0    and $r0.3 = $r0.3, 1   ## bblock 1, line 50,  t19,  t35,  1(I32)
	c0    NOP
	c0    NOP
;;								## 13
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldb $r0.5 = 7[$r0.4]   ## bblock 1, line 48, t9(SI8), t36
;;								## 14
	c0    NOP
	c0    NOP
	c0    NOP
	c0    shl $r0.2 = $r0.2, 1   ## bblock 1, line 49,  t14,  t13(SI8),  1(SI32)
;;								## 15
	c0    or $r0.2 = $r0.2, $r0.3   ## bblock 1, line 50,  t20,  t14,  t19
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 16
	c0    NOP
	c0    add $r0.5 = $r0.5, 1   ## bblock 1, line 48,  t10,  t9(SI8),  1(SI32)
	c0    stb 6[$r0.4] = $r0.2   ## bblock 1, line 50, t36, t20
	c0    NOP
;;								## 17
	c0    NOP
	c0    stb 7[$r0.4] = $r0.5   ## bblock 1, line 48, t36, t10
	c0    return $r0.1 = $r0.1, (0x20), $l0.0   ## bblock 1, line 51,  t23,  ?2.3?2auto_size(I32),  t22
	c0    NOP
;;								## 18
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0x18[$r0.1]  ## restore ## t36
	c0    NOP
;;								## 0
	c0    NOP
	c0    NOP
	c0    ldw $r0.5 = 0x14[$r0.1]  ## restore ## t35
	c0    NOP
;;								## 1
	c0    NOP
	c0    ldw $l0.0 = 0x10[$r0.1]  ## restore ## t22
	c0    NOP
	c0    NOP
;;								## 2
	c0    NOP
	c0    ldb $r0.2 = 6[$r0.4]   ## bblock 1, line 49, t13(SI8), t36
	c0    NOP
	c0    NOP
;;								## 3
	c0    NOP
	c0    NOP
	c0    and $r0.3 = $r0.5, 1   ## bblock 1, line 50,  t19,  t35,  1(I32)
	c0    NOP
;;								## 4
	c0    ldb $r0.5 = 7[$r0.4]   ## bblock 1, line 48, t9(SI8), t36
	c0    goto L5?3 ## goto
	c0    NOP
	c0    NOP
;;								## 5
	c0    add $r0.1 = $r0.1, (-0x20)
	c0    NOP
	c0    ldw $r0.2 = 0[$r0.3]   ## bblock 0, line 55, t2, t16
	c0    NOP
;;								## 0
	c0    stw 0x10[$r0.1] = $l0.0  ## spill ## t4
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 1
	c0    stw 0x14[$r0.1] = $r0.3  ## spill ## t16
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 2
	c0    call $l0.0 = fclose   ## bblock 0, line 55,  raddr(fclose)(P32),  t2
	c0    NOP
	c0    mov $r0.3 = $r0.2   ## t2
	c0    NOP
;;								## 3
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.3 = 0x14[$r0.1]  ## restore ## t16
;;								## 5
	c0    call $l0.0 = free   ## bblock 1, line 56,  raddr(free)(P32),  t16
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 6
	c0    NOP
	c0    ldw $l0.0 = 0x10[$r0.1]  ## restore ## t4
	c0    NOP
	c0    NOP
;;								## 10
	c0    NOP
	c0    NOP
	c0    NOP
	c0    return $r0.1 = $r0.1, (0x20), $l0.0   ## bblock 2, line 57,  t5,  ?2.4?2auto_size(I32),  t4
;;								## 11
	c0    NOP
	c0    NOP
	c0    cmplt $b0.0 = $r0.5, $r0.0   ## bblock 0, line 35,  t178,  t47,  0(I32)
	c0    add $r0.7 = $r0.5, 3   ## bblock 0, line 35,  t179,  t47,  3(I32)
;;								## 0
	c0    slct $r0.7 = $b0.0, $r0.7, $r0.5   ## bblock 0, line 35,  t180,  t178,  t179,  t47
	c0    shr $r0.8 = $r0.5, 2   ## bblock 0, line 35,  t4(SI30),  t47,  2(SI32)
	c0    NOP
	c0    add $r0.9 = $r0.5, 7   ## bblock 0, line 0,  t184,  t47,  7(I32)
;;								## 1
	c0    shr $r0.7 = $r0.7, 2   ## bblock 0, line 35,  t181,  t180,  2(I32)
	c0    sub $r0.9 = $r0.9, $r0.6   ## bblock 0, line 0,  t186,  t184,  t6
	c0    NOP
	c0    mov $r0.6 = $r0.5   ## t47
;;								## 2
	c0    NOP
	c0    NOP
	c0    NOP
	c0    shl $r0.7 = $r0.7, 2   ## bblock 0, line 35,  t182,  t181,  2(I32)
;;								## 3
	c0    NOP
	c0    NOP
	c0    sub $r0.5 = $r0.5, $r0.7   ## bblock 0, line 35,  t1,  t47,  t182
	c0    NOP
;;								## 4
	c0    NOP
	c0    cmpne $b0.0 = $r0.5, $r0.0   ## bblock 0, line 35,  t183(I1),  t1,  0(SI32)
	c0    NOP
	c0    mov $r0.5 = $r0.4   ## t46
;;								## 5
	c0    NOP
	c0    NOP
	c0    slct $r0.4 = $b0.0, $r0.0, 1   ## bblock 0, line 35,  t2,  t183(I1),  0(SI32),  1(SI32)
	c0    NOP
;;								## 6
	c0    NOP
	c0    sub $r0.8 = $r0.8, $r0.4   ## bblock 0, line 0,  t185,  t4(SI30),  t2
	c0    NOP
	c0    NOP
;;								## 7
	c0    NOP
	c0    add $r0.9 = $r0.9, $r0.8   ## bblock 0, line 0,  t128,  t186,  t185
	c0    NOP
	c0    NOP
;;								## 8
	c0    mov $r0.2 = $r0.9   ## bblock 0, line 0,  t127,  t128
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 9
	c0    cmplt $b0.0 = $r0.2, 7   ## bblock 1, line 36,  t187(I1),  t127,  7(SI32)
	c0    cmplt $b0.1 = $r0.2, 6   ## [spec] bblock 6, line 36-1,  t196(I1),  t127,  6(SI32)
	c0    cmplt $b0.2 = $r0.2, 5   ## [spec] bblock 27, line 36-2,  t202(I1),  t127,  5(SI32)
	c0    mov $r0.4 = 32   ## 32(SI32)
;;								## 0
	c0    cmplt $b0.3 = $r0.2, 4   ## [spec] bblock 24, line 36-3,  t201(I1),  t127,  4(SI32)
	c0    cmplt $b0.4 = $r0.2, 3   ## [spec] bblock 21, line 36-4,  t200(I1),  t127,  3(SI32)
	c0    cmplt $b0.5 = $r0.2, 2   ## [spec] bblock 18, line 36-5,  t199(I1),  t127,  2(SI32)
	c0    cmplt $b0.6 = $r0.2, 1   ## [spec] bblock 15, line 36-6,  t198(I1),  t127,  1(SI32)
;;								## 1
	c0    cmplt $b0.0 = $r0.2, $r0.0   ## [spec] bblock 12, line 36-7,  t197(I1),  t127,  0(SI32)
	c0    add $r0.2 = $r0.2, 8   ## [spec] bblock 9, line 0,  t127,  t127,  8(I32)
	c0    brf $b0.0, L1?3   ## bblock 1, line 36,  t187(I1)
	c0    NOP
;;								## 2
	c0    NOP
	c0    stb 0[$r0.3] = $r0.4   ## bblock 6, line 37, t45, 32(SI32)
	c0    NOP
	c0    brf $b0.1, L2?3   ## bblock 6, line 36-1,  t196(I1)
;;								## 3
	c0    NOP
	c0    stb 1[$r0.3] = $r0.4   ## bblock 27, line 37-1, t45, 32(SI32)
	c0    NOP
	c0    brf $b0.2, L3?3   ## bblock 27, line 36-2,  t202(I1)
;;								## 4
	c0    stb 2[$r0.3] = $r0.4   ## bblock 24, line 37-2, t45, 32(SI32)
	c0    NOP
	c0    brf $b0.3, L4?3   ## bblock 24, line 36-3,  t201(I1)
	c0    NOP
;;								## 5
	c0    stb 3[$r0.3] = $r0.4   ## bblock 21, line 37-3, t45, 32(SI32)
	c0    NOP
	c0    brf $b0.4, L5?3   ## bblock 21, line 36-4,  t200(I1)
	c0    NOP
;;								## 6
	c0    NOP
	c0    stb 4[$r0.3] = $r0.4   ## bblock 18, line 37-4, t45, 32(SI32)
	c0    NOP
	c0    brf $b0.5, L6?3   ## bblock 18, line 36-5,  t199(I1)
;;								## 7
	c0    NOP
	c0    stb 5[$r0.3] = $r0.4   ## bblock 15, line 37-5, t45, 32(SI32)
	c0    brf $b0.6, L7?3   ## bblock 15, line 36-6,  t198(I1)
	c0    NOP
;;								## 8
	c0    NOP
	c0    NOP
	c0    stb 6[$r0.3] = $r0.4   ## bblock 12, line 37-6, t45, 32(SI32)
	c0    brf $b0.0, L8?3   ## bblock 12, line 36-7,  t197(I1)
;;								## 9
	c0    stb 7[$r0.3] = $r0.4   ## bblock 9, line 37-7, t45, 32(SI32)
	c0    add $r0.3 = $r0.3, 8   ## bblock 9, line 37-7,  t45,  t45,  8(SI32)
	c0    NOP
	c0    goto L0?3 ## goto
;;								## 10
	c0    add $r0.9 = $r0.6, (~0x3)   ## bblock 23, line 0,  t99,  t47,  (~0x3)(I32)
	c0    add $r0.8 = $r0.6, (~0x2)   ## bblock 23, line 0,  t98,  t47,  (~0x2)(I32)
	c0    add $r0.7 = $r0.6, (~0x1)   ## bblock 23, line 0,  t97,  t47,  (~0x1)(I32)
	c0    mov $r0.2 = $r0.5   ## t46
;;								## 0
	c0    add $r0.5 = $r0.3, 7   ## bblock 10, line 0,  t54,  t45,  7(I32)
	c0    mov $r0.10 = $r0.6   ## t47
	c0    goto L9?3 ## goto
	c0    NOP
;;								## 1
	c0    cmpge $b0.0 = $r0.3, $r0.0   ## bblock 2, line 38,  t188(I1),  t100,  0(SI32)
	c0    shr $r0.4 = $r0.2, $r0.3   ## [spec] bblock 4, line 40,  t18,  t46,  t100
	c0    cmplt $b0.1 = $r0.3, $r0.0   ## [spec] bblock 4, line 41,  t189,  t100,  0(I32)
	c0    add $r0.6 = $r0.5, 1   ## [spec] bblock 4, line 40,  t59,  t54,  1(SI32)
;;								## 0
	c0    and $r0.4 = $r0.4, 1   ## [spec] bblock 4, line 40,  t19,  t18,  1(I32)
	c0    add $r0.11 = $r0.3, 3   ## [spec] bblock 4, line 41,  t190,  t100,  3(I32)
	c0    cmpge $b0.2 = $r0.7, $r0.0   ## [spec] bblock 42, line 38-1,  t227(I1),  t97,  0(SI32)
	c0    mov $r0.12 = 32   ## 32(SI32)
;;								## 1
	c0    add $r0.4 = $r0.4, 48   ## [spec] bblock 4, line 40,  t20,  t19,  48(SI32)
	c0    slct $r0.11 = $b0.1, $r0.11, $r0.3   ## [spec] bblock 4, line 41,  t191,  t189,  t190,  t100
	c0    shr $r0.13 = $r0.2, $r0.7   ## [spec] bblock 43, line 40-1,  t91,  t46,  t97
	c0    brf $b0.0, L11?3   ## bblock 2, line 38,  t188(I1)
;;								## 2
	c0    shr $r0.11 = $r0.11, 2   ## bblock 4, line 41,  t192,  t191,  2(I32)
	c0    and $r0.13 = $r0.13, 1   ## [spec] bblock 43, line 40-1,  t92,  t91,  1(I32)
	c0    cmplt $b0.0 = $r0.7, $r0.0   ## [spec] bblock 43, line 41-1,  t228,  t97,  0(I32)
	c0    add $r0.14 = $r0.7, 3   ## [spec] bblock 43, line 41-1,  t229,  t97,  3(I32)
;;								## 3
	c0    shl $r0.11 = $r0.11, 2   ## bblock 4, line 41,  t193,  t192,  2(I32)
	c0    add $r0.13 = $r0.13, 48   ## [spec] bblock 43, line 40-1,  t93,  t92,  48(SI32)
	c0    slct $r0.14 = $b0.0, $r0.14, $r0.7   ## [spec] bblock 43, line 41-1,  t230,  t228,  t229,  t97
	c0    cmpge $b0.1 = $r0.8, $r0.0   ## [spec] bblock 38, line 38-2,  t219(I1),  t98,  0(SI32)
;;								## 4
	c0    sub $r0.11 = $r0.3, $r0.11   ## bblock 4, line 41,  t25,  t100,  t193
	c0    shr $r0.14 = $r0.14, 2   ## [spec] bblock 43, line 41-1,  t231,  t230,  2(I32)
	c0    shr $r0.15 = $r0.2, $r0.8   ## [spec] bblock 39, line 40-2,  t84,  t46,  t98
	c0    cmplt $b0.0 = $r0.8, $r0.0   ## [spec] bblock 39, line 41-2,  t220,  t98,  0(I32)
;;								## 5
	c0    cmpeq $r0.11 = $r0.11, $r0.0   ## bblock 4, line 41,  t195,  t25,  0(I32)
	c0    shl $r0.14 = $r0.14, 2   ## [spec] bblock 43, line 41-1,  t232,  t231,  2(I32)
	c0    and $r0.15 = $r0.15, 1   ## [spec] bblock 39, line 40-2,  t85,  t84,  1(I32)
	c0    add $r0.16 = $r0.8, 3   ## [spec] bblock 39, line 41-2,  t221,  t98,  3(I32)
;;								## 6
	c0    andl $b0.3 = $r0.11, $r0.3   ## bblock 4, line 41,  t194(I1),  t195,  t100
	c0    sub $r0.14 = $r0.7, $r0.14   ## [spec] bblock 43, line 41-1,  t95,  t97,  t232
	c0    add $r0.15 = $r0.15, 48   ## [spec] bblock 39, line 40-2,  t86,  t85,  48(SI32)
	c0    slct $r0.16 = $b0.0, $r0.16, $r0.8   ## [spec] bblock 39, line 41-2,  t222,  t220,  t221,  t98
;;								## 7
	c0    cmpeq $r0.14 = $r0.14, $r0.0   ## [spec] bblock 43, line 41-1,  t234,  t95,  0(I32)
	c0    shr $r0.16 = $r0.16, 2   ## [spec] bblock 39, line 41-2,  t223,  t222,  2(I32)
	c0    cmpge $b0.0 = $r0.9, $r0.0   ## [spec] bblock 34, line 38-3,  t211(I1),  t99,  0(SI32)
	c0    shr $r0.17 = $r0.2, $r0.9   ## [spec] bblock 35, line 40-3,  t77,  t46,  t99
;;								## 8
	c0    andl $b0.4 = $r0.14, $r0.7   ## [spec] bblock 43, line 41-1,  t233(I1),  t234,  t97
	c0    shl $r0.16 = $r0.16, 2   ## [spec] bblock 39, line 41-2,  t224,  t223,  2(I32)
	c0    and $r0.17 = $r0.17, 1   ## [spec] bblock 35, line 40-3,  t78,  t77,  1(I32)
	c0    cmplt $b0.5 = $r0.9, $r0.0   ## [spec] bblock 35, line 41-3,  t212,  t99,  0(I32)
;;								## 9
	c0    stb 0[$r0.5] = $r0.4   ## bblock 4, line 40, t54, t20
	c0    sub $r0.16 = $r0.8, $r0.16   ## [spec] bblock 39, line 41-2,  t88,  t98,  t224
	c0    add $r0.17 = $r0.17, 48   ## [spec] bblock 35, line 40-3,  t79,  t78,  48(SI32)
	c0    add $r0.11 = $r0.9, 3   ## [spec] bblock 35, line 41-3,  t213,  t99,  3(I32)
;;								## 10
	c0    cmpeq $r0.16 = $r0.16, $r0.0   ## [spec] bblock 39, line 41-2,  t226,  t88,  0(I32)
	c0    NOP
	c0    slct $r0.11 = $b0.5, $r0.11, $r0.9   ## [spec] bblock 35, line 41-3,  t214,  t212,  t213,  t99
	c0    brf $b0.3, L12?3   ## bblock 4, line 41,  t194(I1)
;;								## 11
	c0    stb 1[$r0.5] = $r0.12   ## bblock 5, line 42, t54, 32(SI32)
	c0    add $r0.6 = $r0.5, 2   ## bblock 5, line 42,  t59,  t54,  2(SI32)
	c0    andl $b0.3 = $r0.16, $r0.8   ## [spec] bblock 39, line 41-2,  t225(I1),  t226,  t98
	c0    shr $r0.11 = $r0.11, 2   ## [spec] bblock 35, line 41-3,  t215,  t214,  2(I32)
;;								## 12
	c0    NOP
	c0    add $r0.4 = $r0.6, 1   ## [spec] bblock 43, line 40-1,  t96,  t59,  1(SI32)
	c0    shl $r0.11 = $r0.11, 2   ## [spec] bblock 35, line 41-3,  t216,  t215,  2(I32)
	c0    brf $b0.2, L14?3   ## bblock 42, line 38-1,  t227(I1)
;;								## 13
	c0    stb 0[$r0.6] = $r0.13   ## bblock 43, line 40-1, t59, t93
	c0    sub $r0.11 = $r0.9, $r0.11   ## [spec] bblock 35, line 41-3,  t81,  t99,  t216
	c0    NOP
	c0    brf $b0.4, L15?3   ## bblock 43, line 41-1,  t233(I1)
;;								## 14
	c0    stb 1[$r0.6] = $r0.12   ## bblock 44, line 42-1, t59, 32(SI32)
	c0    add $r0.4 = $r0.6, 2   ## bblock 44, line 42-1,  t96,  t59,  2(SI32)
	c0    cmpeq $r0.11 = $r0.11, $r0.0   ## [spec] bblock 35, line 41-3,  t218,  t81,  0(I32)
	c0    brf $b0.1, L16?3   ## bblock 38, line 38-2,  t219(I1)
;;								## 15
	c0    stb 0[$r0.4] = $r0.15   ## bblock 39, line 40-2, t96, t86
	c0    add $r0.6 = $r0.4, 1   ## bblock 39, line 40-2,  t89,  t96,  1(SI32)
	c0    andl $b0.1 = $r0.11, $r0.9   ## [spec] bblock 35, line 41-3,  t217(I1),  t218,  t99
	c0    brf $b0.3, L18?3   ## bblock 39, line 41-2,  t225(I1)
;;								## 16
	c0    stb 1[$r0.4] = $r0.12   ## bblock 40, line 42-2, t96, 32(SI32)
	c0    add $r0.6 = $r0.4, 2   ## bblock 40, line 42-2,  t89,  t96,  2(SI32)
	c0    NOP
	c0    brf $b0.0, L19?3   ## bblock 34, line 38-3,  t211(I1)
;;								## 17
	c0    stb 0[$r0.6] = $r0.17   ## bblock 35, line 40-3, t89, t79
	c0    add $r0.4 = $r0.6, 1   ## bblock 35, line 40-3,  t82,  t89,  1(SI32)
	c0    brf $b0.1, L21?3   ## bblock 35, line 41-3,  t217(I1)
	c0    NOP
;;								## 18
	c0    add $r0.4 = $r0.6, 2   ## bblock 36, line 42-3,  t82,  t89,  2(SI32)
	c0    add $r0.3 = $r0.3, (~0x4)   ## bblock 30, line 0,  t100,  t100,  (~0x4)(I32)
	c0    add $r0.9 = $r0.9, (~0x4)   ## bblock 30, line 0,  t99,  t99,  (~0x4)(I32)
	c0    add $r0.8 = $r0.8, (~0x4)   ## bblock 30, line 0,  t98,  t98,  (~0x4)(I32)
;;								## 19
	c0    stb 1[$r0.6] = $r0.12   ## bblock 36, line 42-3, t89, 32(SI32)
	c0    add $r0.7 = $r0.7, (~0x4)   ## bblock 30, line 0,  t97,  t97,  (~0x4)(I32)
	c0    add $r0.10 = $r0.10, -5   ## bblock 30, line 38-4,  t47,  t47,  -5(SI32)
	c0    add $r0.5 = $r0.4, 1   ## [spec] bblock 31, line 40-4,  t54,  t82,  1(SI32)
;;								## 20
	c0    cmpge $b0.0 = $r0.10, $r0.0   ## bblock 30, line 38-4,  t203(I1),  t47,  0(SI32)
	c0    shr $r0.6 = $r0.2, $r0.10   ## [spec] bblock 31, line 40-4,  t70,  t46,  t47
	c0    cmplt $b0.1 = $r0.10, $r0.0   ## [spec] bblock 31, line 41-4,  t204,  t47,  0(I32)
	c0    add $r0.11 = $r0.10, 3   ## [spec] bblock 31, line 41-4,  t205,  t47,  3(I32)
;;								## 21
	c0    NOP
	c0    and $r0.6 = $r0.6, 1   ## [spec] bblock 31, line 40-4,  t71,  t70,  1(I32)
	c0    NOP
	c0    slct $r0.11 = $b0.1, $r0.11, $r0.10   ## [spec] bblock 31, line 41-4,  t206,  t204,  t205,  t47
;;								## 22
	c0    add $r0.6 = $r0.6, 48   ## [spec] bblock 31, line 40-4,  t72,  t71,  48(SI32)
	c0    shr $r0.11 = $r0.11, 2   ## [spec] bblock 31, line 41-4,  t207,  t206,  2(I32)
	c0    brf $b0.0, L23?3   ## bblock 30, line 38-4,  t203(I1)
	c0    NOP
;;								## 23
	c0    NOP
	c0    stb 0[$r0.4] = $r0.6   ## bblock 31, line 40-4, t82, t72
	c0    NOP
	c0    shl $r0.11 = $r0.11, 2   ## bblock 31, line 41-4,  t208,  t207,  2(I32)
;;								## 24
	c0    sub $r0.11 = $r0.10, $r0.11   ## bblock 31, line 41-4,  t74,  t47,  t208
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 25
	c0    cmpeq $r0.11 = $r0.11, $r0.0   ## bblock 31, line 41-4,  t210,  t74,  0(I32)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 26
	c0    NOP
	c0    andl $b0.0 = $r0.11, $r0.10   ## bblock 31, line 41-4,  t209(I1),  t210,  t47
	c0    NOP
	c0    NOP
;;								## 28
	c0    brf $b0.0, L10?3   ## bblock 31, line 41-4,  t209(I1)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 29
	c0    stb 1[$r0.4] = $r0.12   ## bblock 32, line 42-4, t82, 32(SI32)
	c0    add $r0.5 = $r0.4, 2   ## bblock 32, line 42-4,  t54,  t82,  2(SI32)
	c0    NOP
	c0    goto L10?3 ## goto
;;								## 30
	c0    NOP
	c0    mov $r0.5 = $r0.4   ## bblock 33, line 0,  t75,  t82
	c0    NOP
	c0    goto L24?3 ## goto
;;								## 0
	c0    add $r0.3 = $r0.3, (~0x4)   ## bblock 30, line 0,  t100,  t100,  (~0x4)(I32)
	c0    add $r0.9 = $r0.9, (~0x4)   ## bblock 30, line 0,  t99,  t99,  (~0x4)(I32)
	c0    add $r0.8 = $r0.8, (~0x4)   ## bblock 30, line 0,  t98,  t98,  (~0x4)(I32)
	c0    add $r0.7 = $r0.7, (~0x4)   ## bblock 30, line 0,  t97,  t97,  (~0x4)(I32)
;;								## 0
	c0    add $r0.10 = $r0.10, -5   ## bblock 30, line 38-4,  t47,  t47,  -5(SI32)
	c0    add $r0.5 = $r0.4, 1   ## [spec] bblock 31, line 40-4,  t54,  t82,  1(SI32)
	c0    mov $r0.12 = 32   ## 32(SI32)
	c0    goto L22?3 ## goto
;;								## 1
	c0    mov $r0.5 = $r0.6   ## bblock 37, line 0,  t75,  t89
	c0    NOP
	c0    goto L24?3 ## goto
	c0    NOP
;;								## 0
	c0    add $r0.11 = $r0.9, 3   ## [spec] bblock 35, line 41-3,  t213,  t99,  3(I32)
	c0    mov $r0.12 = 32   ## 32(SI32)
	c0    brf $b0.0, L19?3   ## bblock 34, line 38-3,  t211(I1)
	c0    NOP
;;								## 0
	c0    NOP
	c0    NOP
	c0    slct $r0.11 = $b0.5, $r0.11, $r0.9   ## bblock 35, line 41-3,  t214,  t212,  t213,  t99
	c0    NOP
;;								## 1
	c0    NOP
	c0    shr $r0.11 = $r0.11, 2   ## bblock 35, line 41-3,  t215,  t214,  2(I32)
	c0    NOP
	c0    NOP
;;								## 2
	c0    NOP
	c0    NOP
	c0    NOP
	c0    shl $r0.11 = $r0.11, 2   ## bblock 35, line 41-3,  t216,  t215,  2(I32)
;;								## 3
	c0    NOP
	c0    NOP
	c0    NOP
	c0    sub $r0.11 = $r0.9, $r0.11   ## bblock 35, line 41-3,  t81,  t99,  t216
;;								## 4
	c0    cmpeq $r0.11 = $r0.11, $r0.0   ## bblock 35, line 41-3,  t218,  t81,  0(I32)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 5
	c0    NOP
	c0    NOP
	c0    NOP
	c0    andl $b0.1 = $r0.11, $r0.9   ## bblock 35, line 41-3,  t217(I1),  t218,  t99
;;								## 6
	c0    goto L20?3 ## goto
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 7
	c0    NOP
	c0    mov $r0.5 = $r0.4   ## bblock 41, line 0,  t75,  t96
	c0    goto L24?3 ## goto
	c0    NOP
;;								## 0
	c0    cmpeq $r0.11 = $r0.11, $r0.0   ## [spec] bblock 35, line 41-3,  t218,  t81,  0(I32)
	c0    NOP
	c0    mov $r0.12 = 32   ## 32(SI32)
	c0    brf $b0.1, L16?3   ## bblock 38, line 38-2,  t219(I1)
;;
	c0    NOP
	c0    goto L17?3 ## goto
	c0    NOP
	c0    NOP
;;								## 0
	c0    NOP
	c0    mov $r0.5 = $r0.6   ## bblock 45, line 0,  t75,  t59
	c0    NOP
	c0    goto L24?3 ## goto
;;								## 0
	c0    shr $r0.11 = $r0.11, 2   ## [spec] bblock 35, line 41-3,  t215,  t214,  2(I32)
	c0    mov $r0.12 = 32   ## 32(SI32)
	c0    andl $b0.3 = $r0.16, $r0.8   ## [spec] bblock 39, line 41-2,  t225(I1),  t226,  t98
	c0    goto L13?3 ## goto
;;								## 0
	c0    NOP
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 0
	c0    NOP
	c0    NOP
	c0    stb 0[$r0.5] = $r0.0   ## bblock 3, line 44, t75, 0(SI32)
	c0    return $r0.1 = $r0.1, (0x0), $l0.0   ## bblock 3, line 45,  t31,  ?2.1?2auto_size(I32),  t30
;;								## 1
	c0    add $r0.9 = $r0.6, (~0x3)   ## bblock 23, line 0,  t99,  t47,  (~0x3)(I32)
	c0    add $r0.8 = $r0.6, (~0x2)   ## bblock 23, line 0,  t98,  t47,  (~0x2)(I32)
	c0    add $r0.7 = $r0.6, (~0x1)   ## bblock 23, line 0,  t97,  t47,  (~0x1)(I32)
	c0    mov $r0.2 = $r0.5   ## t46
;;								## 0
	c0    add $r0.5 = $r0.3, 6   ## bblock 13, line 0,  t54,  t45,  6(I32)
	c0    mov $r0.10 = $r0.6   ## t47
	c0    NOP
	c0    goto L9?3 ## goto
;;								## 1
	c0    add $r0.9 = $r0.6, (~0x3)   ## bblock 23, line 0,  t99,  t47,  (~0x3)(I32)
	c0    add $r0.8 = $r0.6, (~0x2)   ## bblock 23, line 0,  t98,  t47,  (~0x2)(I32)
	c0    add $r0.7 = $r0.6, (~0x1)   ## bblock 23, line 0,  t97,  t47,  (~0x1)(I32)
	c0    mov $r0.2 = $r0.5   ## t46
;;								## 0
	c0    NOP
	c0    add $r0.5 = $r0.3, 5   ## bblock 16, line 0,  t54,  t45,  5(I32)
	c0    mov $r0.10 = $r0.6   ## t47
	c0    goto L9?3 ## goto
;;								## 1
	c0    add $r0.9 = $r0.6, (~0x3)   ## bblock 23, line 0,  t99,  t47,  (~0x3)(I32)
	c0    add $r0.8 = $r0.6, (~0x2)   ## bblock 23, line 0,  t98,  t47,  (~0x2)(I32)
	c0    add $r0.7 = $r0.6, (~0x1)   ## bblock 23, line 0,  t97,  t47,  (~0x1)(I32)
	c0    mov $r0.2 = $r0.5   ## t46
;;								## 0
	c0    NOP
	c0    add $r0.5 = $r0.3, 4   ## bblock 19, line 0,  t54,  t45,  4(I32)
	c0    mov $r0.10 = $r0.6   ## t47
	c0    goto L9?3 ## goto
;;								## 1
	c0    add $r0.9 = $r0.6, (~0x3)   ## bblock 23, line 0,  t99,  t47,  (~0x3)(I32)
	c0    add $r0.8 = $r0.6, (~0x2)   ## bblock 23, line 0,  t98,  t47,  (~0x2)(I32)
	c0    add $r0.7 = $r0.6, (~0x1)   ## bblock 23, line 0,  t97,  t47,  (~0x1)(I32)
	c0    mov $r0.2 = $r0.5   ## t46
;;								## 0
	c0    add $r0.5 = $r0.3, 3   ## bblock 22, line 0,  t54,  t45,  3(I32)
	c0    mov $r0.10 = $r0.6   ## t47
	c0    NOP
	c0    goto L9?3 ## goto
;;								## 1
	c0    add $r0.9 = $r0.6, (~0x3)   ## bblock 23, line 0,  t99,  t47,  (~0x3)(I32)
	c0    add $r0.8 = $r0.6, (~0x2)   ## bblock 23, line 0,  t98,  t47,  (~0x2)(I32)
	c0    add $r0.7 = $r0.6, (~0x1)   ## bblock 23, line 0,  t97,  t47,  (~0x1)(I32)
	c0    mov $r0.2 = $r0.5   ## t46
;;								## 0
	c0    NOP
	c0    add $r0.5 = $r0.3, 2   ## bblock 25, line 0,  t54,  t45,  2(I32)
	c0    mov $r0.10 = $r0.6   ## t47
	c0    goto L9?3 ## goto
;;								## 1
	c0    add $r0.9 = $r0.6, (~0x3)   ## bblock 23, line 0,  t99,  t47,  (~0x3)(I32)
	c0    add $r0.8 = $r0.6, (~0x2)   ## bblock 23, line 0,  t98,  t47,  (~0x2)(I32)
	c0    add $r0.7 = $r0.6, (~0x1)   ## bblock 23, line 0,  t97,  t47,  (~0x1)(I32)
	c0    mov $r0.2 = $r0.5   ## t46
;;								## 0
	c0    add $r0.5 = $r0.3, 1   ## bblock 28, line 0,  t54,  t45,  1(I32)
	c0    NOP
	c0    mov $r0.10 = $r0.6   ## t47
	c0    goto L9?3 ## goto
;;								## 1
	c0    add $r0.9 = $r0.6, (~0x3)   ## bblock 23, line 0,  t99,  t47,  (~0x3)(I32)
	c0    add $r0.8 = $r0.6, (~0x2)   ## bblock 23, line 0,  t98,  t47,  (~0x2)(I32)
	c0    add $r0.7 = $r0.6, (~0x1)   ## bblock 23, line 0,  t97,  t47,  (~0x1)(I32)
	c0    mov $r0.2 = $r0.5   ## t46
;;								## 0
	c0    mov $r0.5 = $r0.3   ## bblock 29, line 0,  t54,  t45
	c0    NOP
	c0    mov $r0.10 = $r0.6   ## t47
	c0    NOP
;;								## 1
	c0    add $r0.3 = $r0.6, (~0x0)   ## bblock 23, line 0,  t100,  t47,  (~0x0)(I32)
	c0    goto L10?3 ## goto
	c0    NOP
	c0    NOP
;;								## 2
	c0    add $r0.1 = $r0.1, (-0x20)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 0
	c0    NOP
	c0    stw 0x10[$r0.1] = $l0.0  ## spill ## t19
	c0    NOP
	c0    NOP
;;								## 1
	c0    stw 0x14[$r0.1] = $r0.57  ## spill ## t23
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 2
	c0    mov $r0.57 = $r0.3   ## t31
	c0    stw 0x18[$r0.1] = $r0.58  ## spill ## t24
	c0    NOP
	c0    NOP
;;								## 3
	c0    NOP
	c0    NOP
	c0    mov $r0.58 = $r0.0   ## bblock 0, line 14,  t36,  0(SI32)
	c0    NOP
;;								## 4
	c0    NOP
	c0    ldb.d $r0.4 = 0[$r0.57]   ## [spec] bblock 1, line 16, t2(SI8), t31
	c0    NOP
	c0    mov $r0.3 = (_?1STRINGPACKET.1 + 0)   ## addr(_?1STRINGVAR.1)(P32)
;;								## 2
	c0    NOP
	c0    NOP
	c0    NOP
	c0    andl $b0.0 = $r0.57, $r0.4   ## bblock 1, line 16,  t37(I1),  t31,  t2(SI8)
;;								## 4
	c0    NOP
	c0    NOP
	c0    NOP
	c0    brf $b0.0, L1?3   ## bblock 1, line 16,  t37(I1)
;;								## 5
	c0    call $l0.0 = strchr   ## bblock 4, line 16,  raddr(strchr)(P32),  addr(_?1STRINGVAR.1)(P32),  t2(SI8)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 6
	c0    cmpne $b0.0 = $r0.3, $r0.0   ## bblock 5, line 16,  t38(I1),  t3,  0x0(P32)
	c0    ldb.d $r0.2 = 0[$r0.57]   ## [spec] bblock 6, line 18, t10(SI8), t31
	c0    shl $r0.3 = $r0.58, 1   ## [spec] bblock 6, line 19,  t14,  t36,  1(SI32)
	c0    add $r0.57 = $r0.57, 1   ## [spec] bblock 6, line 18,  t31,  t31,  1(SI32)
;;								## 8
	c0    NOP
	c0    NOP
	c0    NOP
	c0    brf $b0.0, L2?3   ## bblock 5, line 16,  t38(I1)
;;								## 9
	c0    NOP
	c0    NOP
	c0    NOP
	c0    add $r0.2 = $r0.2, -48   ## bblock 6, line 18,  t15,  t10(SI8),  -48(SI32)
;;								## 10
	c0    NOP
	c0    and $r0.2 = $r0.2, 1   ## bblock 6, line 20,  t16,  t15,  1(I32)
	c0    NOP
	c0    NOP
;;								## 11
	c0    NOP
	c0    or $r0.58 = $r0.3, $r0.2   ## bblock 6, line 20,  t36,  t14,  t16
	c0    goto L0?3 ## goto
	c0    NOP
;;								## 12
	c0    NOP
	c0    mov $r0.3 = $r0.58   ## bblock 7, line 0,  t32,  t36
	c0    NOP
	c0    ldw $l0.0 = 0x10[$r0.1]  ## restore ## t19
;;								## 0
	c0    NOP
	c0    NOP
	c0    ldw $r0.58 = 0x18[$r0.1]  ## restore ## t24
	c0    NOP
;;								## 1
	c0    NOP
	c0    NOP
	c0    ldw $r0.57 = 0x14[$r0.1]  ## restore ## t23
	c0    NOP
;;								## 2
	c0    goto L3?3 ## goto
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 3
	c0    NOP
	c0    NOP
	c0    mov $r0.3 = $r0.58   ## bblock 8, line 0,  t32,  t36
	c0    ldw $l0.0 = 0x10[$r0.1]  ## restore ## t19
;;								## 0
	c0    ldw $r0.58 = 0x18[$r0.1]  ## restore ## t24
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 1
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.57 = 0x14[$r0.1]  ## restore ## t23
;;								## 3
	c0    NOP
	c0    NOP
	c0    NOP
	c0    return $r0.1 = $r0.1, (0x20), $l0.0   ## bblock 2, line 22,  t20,  ?2.1?2auto_size(I32),  t19
;;								## 4
