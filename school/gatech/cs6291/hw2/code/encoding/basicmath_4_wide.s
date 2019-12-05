	c0    NOP
	c0    add $r0.1 = $r0.1, (-0x80)
	c0    NOP
	c0    mov $r0.3 = (_?1STRINGPACKET.1 + 0)   ## addr(_?1STRINGVAR.1)(P32)
;;								## 0
	c0    NOP
	c0    NOP
	c0    add $r0.2 = $r0.1, 0x18   ## bblock 0, line 9,  t274,  t441,  offset(x?1.2?2hi.0)=0x18(P32)
	c0    stw 0x3c[$r0.1] = $l0.0  ## spill ## t440
;;								## 1
	c0    NOP
	c0    NOP
	c0    stw 0x5c[$r0.1] = $r0.57  ## spill ## t444
	c0    NOP
;;								## 2
	c0    NOP
	c0    stw 0x60[$r0.1] = $r0.58  ## spill ## t445
	c0    NOP
	c0    NOP
;;								## 3
	c0    NOP
	c0    stw 0x64[$r0.1] = $r0.59  ## spill ## t446
	c0    NOP
	c0    NOP
;;								## 4
	c0    NOP
	c0    add $r0.59 = $r0.1, 0x1c   ## bblock 0, line 9,  t275,  t441,  offset(x?1.2?2lo.1)=0x1c(P32)
	c0    call $l0.0 = printf   ## bblock 0, line 18,  raddr(printf)(P32),  addr(_?1STRINGVAR.1)(P32)
	c0    stw 0x68[$r0.1] = $r0.2  ## spill ## t274
;;								## 5
	c0    add $r0.2 = $r0.1, 0x30   ## bblock 1, line 20,  t8,  t441,  offset(solutions?1.2)=0x30(P32)
	c0    mov $r0.4 = $r0.0   ## 0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    ldw $r0.58 = 0x68[$r0.1]  ## restore ## t274
;;								## 6
	c0    mov $r0.8 = $r0.0   ## 0(I32)
	c0    NOP
	c0    mov $r0.10 = $r0.0   ## 0(I32)
	c0    mov $r0.3 = 1072693248   ## 1072693248(I32)
;;								## 7
	c0    mov $r0.5 = (~0x3fdaffff)   ## (~0x3fdaffff)(I32)
	c0    NOP
	c0    mov $r0.7 = 1077936128   ## 1077936128(I32)
	c0    NOP
;;								## 8
	c0    stw 0x10[$r0.1] = $r0.2   ## bblock 1, line 20, t441, t8
	c0    NOP
	c0    NOP
	c0    mov $r0.9 = (~0x3fc1ffff)   ## (~0x3fc1ffff)(I32)
;;								## 9
	c0    NOP
	c0    NOP
	c0    stw 0x14[$r0.1] = $r0.58   ## bblock 1, line 20, t441, t274
	c0    call $l0.0 = SolveCubic   ## bblock 1, line 20,  raddr(SolveCubic)(P32),  1072693248(I32),  0(I32),  (~0x3fdaffff)(I32),  0(I32),  1077936128(I32),  0(I32),  (~0x3fc1ffff)(I32),  0(I32)
;;								## 10
	c0    call $l0.0 = printf   ## bblock 2, line 21,  raddr(printf)(P32),  addr(_?1STRINGVAR.2)(P32)
	c0    NOP
	c0    NOP
	c0    mov $r0.3 = (_?1STRINGPACKET.2 + 0)   ## addr(_?1STRINGVAR.2)(P32)
;;								## 11
	c0    NOP
	c0    mov $r0.57 = $r0.0   ## bblock 3, line 15,  t453,  0(SI32)
	c0    ldw $r0.58 = 0x68[$r0.1]  ## restore ## t274
	c0    NOP
;;								## 13
	c0    NOP
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 14
	c0    ldw $r0.2 = 0x30[$r0.1]   ## bblock 4, line 22, t14, t441
	c0    sh3add $r0.7 = $r0.57, $r0.58   ## [spec] bblock 101, line 23,  t571,  t453,  t274
	c0    sh3add $r0.8 = $r0.57, $r0.59   ## [spec] bblock 101, line 23,  t572,  t453,  t275
	c0    mov $r0.4 = $r0.0   ## 0(I32)
;;								## 0
	c0    NOP
	c0    NOP
	c0    ldw.d $r0.5 = 0[$r0.7]   ## [spec] bblock 101, line 23, t438, t571
	c0    mov $r0.3 = (_?1STRINGPACKET.3 + 0)   ## addr(_?1STRINGVAR.3)(P32)
;;								## 1
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw.d $r0.6 = 0[$r0.8]   ## [spec] bblock 101, line 23, t439, t572
;;								## 2
	c0    NOP
	c0    NOP
	c0    cmplt $b0.0 = $r0.57, $r0.2   ## bblock 4, line 22,  t538(I1),  t453,  t14
	c0    NOP
;;								## 4
	c0    NOP
	c0    NOP
	c0    brf $b0.0, L1?3   ## bblock 4, line 22,  t538(I1)
	c0    NOP
;;								## 5
	c0    NOP
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 101, line 23,  raddr(printf)(P32),  addr(_?1STRINGVAR.3)(P32),  0(I32),  t438,  t439
	c0    NOP
;;								## 6
	c0    NOP
	c0    add $r0.57 = $r0.57, 1   ## bblock 102, line 22,  t453,  t453,  1(SI32)
	c0    goto L0?3 ## goto
	c0    NOP
;;								## 7
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 5, line 24,  raddr(printf)(P32),  addr(_?1STRINGVAR.4)(P32)
	c0    mov $r0.3 = (_?1STRINGPACKET.4 + 0)   ## addr(_?1STRINGVAR.4)(P32)
	c0    NOP
;;								## 0
	c0    add $r0.2 = $r0.1, 0x30   ## bblock 6, line 28,  t29,  t441,  offset(solutions?1.2)=0x30(P32)
	c0    mov $r0.4 = $r0.0   ## 0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.8 = $r0.0   ## 0(I32)
;;								## 1
	c0    stw 0x10[$r0.1] = $r0.2   ## bblock 6, line 28, t441, t29
	c0    NOP
	c0    mov $r0.10 = $r0.0   ## 0(I32)
	c0    mov $r0.3 = 1072693248   ## 1072693248(I32)
;;								## 2
	c0    NOP
	c0    mov $r0.5 = (~0x3fedffff)   ## (~0x3fedffff)(I32)
	c0    mov $r0.7 = 1076953088   ## 1076953088(I32)
	c0    NOP
;;								## 3
	c0    NOP
	c0    stw 0x14[$r0.1] = $r0.58   ## bblock 6, line 28, t441, t274
	c0    call $l0.0 = SolveCubic   ## bblock 6, line 28,  raddr(SolveCubic)(P32),  1072693248(I32),  0(I32),  (~0x3fedffff)(I32),  0(I32),  1076953088(I32),  0(I32),  (~0x3fc1ffff)(I32),  0(I32)
	c0    mov $r0.9 = (~0x3fc1ffff)   ## (~0x3fc1ffff)(I32)
;;								## 4
	c0    NOP
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 7, line 29,  raddr(printf)(P32),  addr(_?1STRINGVAR.5)(P32)
	c0    mov $r0.3 = (_?1STRINGPACKET.5 + 0)   ## addr(_?1STRINGVAR.5)(P32)
;;								## 5
	c0    NOP
	c0    mov $r0.57 = $r0.0   ## bblock 8, line 30,  t482,  0(SI32)
	c0    NOP
	c0    NOP
;;								## 6
	c0    ldw $r0.2 = 0x30[$r0.1]   ## bblock 9, line 30, t35, t441
	c0    sh3add $r0.7 = $r0.57, $r0.58   ## [spec] bblock 99, line 31,  t569,  t482,  t274
	c0    sh3add $r0.8 = $r0.57, $r0.59   ## [spec] bblock 99, line 31,  t570,  t482,  t275
	c0    mov $r0.4 = $r0.0   ## 0(I32)
;;								## 0
	c0    ldw.d $r0.5 = 0[$r0.7]   ## [spec] bblock 99, line 31, t436, t569
	c0    mov $r0.3 = (_?1STRINGPACKET.6 + 0)   ## addr(_?1STRINGVAR.6)(P32)
	c0    NOP
	c0    NOP
;;								## 1
	c0    ldw.d $r0.6 = 0[$r0.8]   ## [spec] bblock 99, line 31, t437, t570
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 2
	c0    cmplt $b0.0 = $r0.57, $r0.2   ## bblock 9, line 30,  t539(I1),  t482,  t35
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 4
	c0    NOP
	c0    brf $b0.0, L3?3   ## bblock 9, line 30,  t539(I1)
	c0    NOP
	c0    NOP
;;								## 5
	c0    NOP
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 99, line 31,  raddr(printf)(P32),  addr(_?1STRINGVAR.6)(P32),  0(I32),  t436,  t437
	c0    NOP
;;								## 6
	c0    NOP
	c0    NOP
	c0    add $r0.57 = $r0.57, 1   ## bblock 100, line 30,  t482,  t482,  1(SI32)
	c0    goto L2?3 ## goto
;;								## 7
	c0    NOP
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 10, line 32,  raddr(printf)(P32),  addr(_?1STRINGVAR.7)(P32)
	c0    mov $r0.3 = (_?1STRINGPACKET.7 + 0)   ## addr(_?1STRINGVAR.7)(P32)
;;								## 0
	c0    add $r0.2 = $r0.1, 0x30   ## bblock 11, line 35,  t50,  t441,  offset(solutions?1.2)=0x30(P32)
	c0    mov $r0.4 = $r0.0   ## 0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.8 = $r0.0   ## 0(I32)
;;								## 1
	c0    NOP
	c0    stw 0x10[$r0.1] = $r0.2   ## bblock 11, line 35, t441, t50
	c0    mov $r0.10 = $r0.0   ## 0(I32)
	c0    mov $r0.3 = 1072693248   ## 1072693248(I32)
;;								## 2
	c0    mov $r0.5 = (~0x3ff3ffff)   ## (~0x3ff3ffff)(I32)
	c0    NOP
	c0    mov $r0.7 = 1077280768   ## 1077280768(I32)
	c0    NOP
;;								## 3
	c0    NOP
	c0    stw 0x14[$r0.1] = $r0.58   ## bblock 11, line 35, t441, t274
	c0    call $l0.0 = SolveCubic   ## bblock 11, line 35,  raddr(SolveCubic)(P32),  1072693248(I32),  0(I32),  (~0x3ff3ffff)(I32),  0(I32),  1077280768(I32),  0(I32),  (~0x3fc0ffff)(I32),  0(I32)
	c0    mov $r0.9 = (~0x3fc0ffff)   ## (~0x3fc0ffff)(I32)
;;								## 4
	c0    call $l0.0 = printf   ## bblock 12, line 36,  raddr(printf)(P32),  addr(_?1STRINGVAR.8)(P32)
	c0    mov $r0.3 = (_?1STRINGPACKET.8 + 0)   ## addr(_?1STRINGVAR.8)(P32)
	c0    NOP
	c0    NOP
;;								## 5
	c0    NOP
	c0    NOP
	c0    NOP
	c0    mov $r0.57 = $r0.0   ## bblock 13, line 37,  t483,  0(SI32)
;;								## 6
	c0    ldw $r0.2 = 0x30[$r0.1]   ## bblock 14, line 37, t56, t441
	c0    sh3add $r0.7 = $r0.57, $r0.58   ## [spec] bblock 97, line 38,  t567,  t483,  t274
	c0    sh3add $r0.8 = $r0.57, $r0.59   ## [spec] bblock 97, line 38,  t568,  t483,  t275
	c0    mov $r0.4 = $r0.0   ## 0(I32)
;;								## 0
	c0    ldw.d $r0.5 = 0[$r0.7]   ## [spec] bblock 97, line 38, t434, t567
	c0    NOP
	c0    NOP
	c0    mov $r0.3 = (_?1STRINGPACKET.9 + 0)   ## addr(_?1STRINGVAR.9)(P32)
;;								## 1
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw.d $r0.6 = 0[$r0.8]   ## [spec] bblock 97, line 38, t435, t568
;;								## 2
	c0    NOP
	c0    cmplt $b0.0 = $r0.57, $r0.2   ## bblock 14, line 37,  t540(I1),  t483,  t56
	c0    NOP
	c0    NOP
;;								## 4
	c0    brf $b0.0, L5?3   ## bblock 14, line 37,  t540(I1)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 5
	c0    call $l0.0 = printf   ## bblock 97, line 38,  raddr(printf)(P32),  addr(_?1STRINGVAR.9)(P32),  0(I32),  t434,  t435
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 6
	c0    NOP
	c0    NOP
	c0    add $r0.57 = $r0.57, 1   ## bblock 98, line 37,  t483,  t483,  1(SI32)
	c0    goto L4?3 ## goto
;;								## 7
	c0    call $l0.0 = printf   ## bblock 15, line 39,  raddr(printf)(P32),  addr(_?1STRINGVAR.10)(P32)
	c0    NOP
	c0    NOP
	c0    mov $r0.3 = (_?1STRINGPACKET.10 + 0)   ## addr(_?1STRINGVAR.10)(P32)
;;								## 0
	c0    add $r0.2 = $r0.1, 0x30   ## bblock 16, line 42,  t71,  t441,  offset(solutions?1.2)=0x30(P32)
	c0    mov $r0.4 = $r0.0   ## 0(I32)
	c0    mov $r0.8 = $r0.0   ## 0(I32)
	c0    mov $r0.10 = $r0.0   ## 0(I32)
;;								## 1
	c0    NOP
	c0    mov $r0.3 = 1072693248   ## 1072693248(I32)
	c0    mov $r0.7 = 1072693248   ## 1072693248(I32)
	c0    NOP
;;								## 2
	c0    mov $r0.5 = (~0x3fd49999)   ## (~0x3fd49999)(I32)
	c0    NOP
	c0    mov $r0.6 = 1717986918   ## 1717986918(I32)
	c0    NOP
;;								## 3
	c0    NOP
	c0    NOP
	c0    stw 0x10[$r0.1] = $r0.2   ## bblock 16, line 42, t441, t71
	c0    mov $r0.9 = (~0x3fbe7fff)   ## (~0x3fbe7fff)(I32)
;;								## 4
	c0    NOP
	c0    stw 0x14[$r0.1] = $r0.58   ## bblock 16, line 42, t441, t274
	c0    NOP
	c0    call $l0.0 = SolveCubic   ## bblock 16, line 42,  raddr(SolveCubic)(P32),  1072693248(I32),  0(I32),  (~0x3fd49999)(I32),  1717986918(I32),  1072693248(I32),  0(I32),  (~0x3fbe7fff)(I32),  0(I32)
;;								## 5
	c0    NOP
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 17, line 43,  raddr(printf)(P32),  addr(_?1STRINGVAR.11)(P32)
	c0    mov $r0.3 = (_?1STRINGPACKET.11 + 0)   ## addr(_?1STRINGVAR.11)(P32)
;;								## 6
	c0    NOP
	c0    NOP
	c0    NOP
	c0    mov $r0.57 = $r0.0   ## bblock 18, line 44,  t484,  0(SI32)
;;								## 7
	c0    ldw $r0.2 = 0x30[$r0.1]   ## bblock 19, line 44, t77, t441
	c0    sh3add $r0.7 = $r0.57, $r0.58   ## [spec] bblock 95, line 45,  t565,  t484,  t274
	c0    sh3add $r0.8 = $r0.57, $r0.59   ## [spec] bblock 95, line 45,  t566,  t484,  t275
	c0    mov $r0.4 = $r0.0   ## 0(I32)
;;								## 0
	c0    ldw.d $r0.5 = 0[$r0.7]   ## [spec] bblock 95, line 45, t432, t565
	c0    NOP
	c0    NOP
	c0    mov $r0.3 = (_?1STRINGPACKET.12 + 0)   ## addr(_?1STRINGVAR.12)(P32)
;;								## 1
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw.d $r0.6 = 0[$r0.8]   ## [spec] bblock 95, line 45, t433, t566
;;								## 2
	c0    NOP
	c0    NOP
	c0    cmplt $b0.0 = $r0.57, $r0.2   ## bblock 19, line 44,  t541(I1),  t484,  t77
	c0    NOP
;;								## 4
	c0    NOP
	c0    brf $b0.0, L7?3   ## bblock 19, line 44,  t541(I1)
	c0    NOP
	c0    NOP
;;								## 5
	c0    NOP
	c0    NOP
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 95, line 45,  raddr(printf)(P32),  addr(_?1STRINGVAR.12)(P32),  0(I32),  t432,  t433
;;								## 6
	c0    add $r0.57 = $r0.57, 1   ## bblock 96, line 44,  t484,  t484,  1(SI32)
	c0    goto L6?3 ## goto
	c0    NOP
	c0    NOP
;;								## 7
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 20, line 46,  raddr(printf)(P32),  addr(_?1STRINGVAR.13)(P32)
	c0    mov $r0.3 = (_?1STRINGPACKET.13 + 0)   ## addr(_?1STRINGVAR.13)(P32)
	c0    NOP
;;								## 0
	c0    add $r0.2 = $r0.1, 0x30   ## bblock 21, line 49,  t92,  t441,  offset(solutions?1.2)=0x30(P32)
	c0    mov $r0.4 = $r0.0   ## 0(I32)
	c0    mov $r0.8 = $r0.0   ## 0(I32)
	c0    mov $r0.10 = $r0.0   ## 0(I32)
;;								## 1
	c0    NOP
	c0    mov $r0.3 = 1074266112   ## 1074266112(I32)
	c0    mov $r0.5 = 1076407828   ## 1076407828(I32)
	c0    NOP
;;								## 2
	c0    mov $r0.6 = 2061584302   ## 2061584302(I32)
	c0    NOP
	c0    mov $r0.7 = 1075052544   ## 1075052544(I32)
	c0    NOP
;;								## 3
	c0    NOP
	c0    NOP
	c0    stw 0x10[$r0.1] = $r0.2   ## bblock 21, line 49, t441, t92
	c0    mov $r0.9 = 1076363264   ## 1076363264(I32)
;;								## 4
	c0    stw 0x14[$r0.1] = $r0.58   ## bblock 21, line 49, t441, t274
	c0    call $l0.0 = SolveCubic   ## bblock 21, line 49,  raddr(SolveCubic)(P32),  1074266112(I32),  0(I32),  1076407828(I32),  2061584302(I32),  1075052544(I32),  0(I32),  1076363264(I32),  0(I32)
	c0    NOP
	c0    NOP
;;								## 5
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 22, line 50,  raddr(printf)(P32),  addr(_?1STRINGVAR.14)(P32)
	c0    NOP
	c0    mov $r0.3 = (_?1STRINGPACKET.14 + 0)   ## addr(_?1STRINGVAR.14)(P32)
;;								## 6
	c0    NOP
	c0    NOP
	c0    mov $r0.57 = $r0.0   ## bblock 23, line 51,  t485,  0(SI32)
	c0    NOP
;;								## 7
	c0    ldw $r0.2 = 0x30[$r0.1]   ## bblock 24, line 51, t98, t441
	c0    sh3add $r0.7 = $r0.57, $r0.58   ## [spec] bblock 93, line 52,  t563,  t485,  t274
	c0    sh3add $r0.8 = $r0.57, $r0.59   ## [spec] bblock 93, line 52,  t564,  t485,  t275
	c0    mov $r0.4 = $r0.0   ## 0(I32)
;;								## 0
	c0    NOP
	c0    NOP
	c0    ldw.d $r0.5 = 0[$r0.7]   ## [spec] bblock 93, line 52, t430, t563
	c0    mov $r0.3 = (_?1STRINGPACKET.15 + 0)   ## addr(_?1STRINGVAR.15)(P32)
;;								## 1
	c0    ldw.d $r0.6 = 0[$r0.8]   ## [spec] bblock 93, line 52, t431, t564
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 2
	c0    NOP
	c0    NOP
	c0    cmplt $b0.0 = $r0.57, $r0.2   ## bblock 24, line 51,  t542(I1),  t485,  t98
	c0    NOP
;;								## 4
	c0    NOP
	c0    NOP
	c0    NOP
	c0    brf $b0.0, L9?3   ## bblock 24, line 51,  t542(I1)
;;								## 5
	c0    NOP
	c0    NOP
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 93, line 52,  raddr(printf)(P32),  addr(_?1STRINGVAR.15)(P32),  0(I32),  t430,  t431
;;								## 6
	c0    add $r0.57 = $r0.57, 1   ## bblock 94, line 51,  t485,  t485,  1(SI32)
	c0    NOP
	c0    NOP
	c0    goto L8?3 ## goto
;;								## 7
	c0    call $l0.0 = printf   ## bblock 25, line 53,  raddr(printf)(P32),  addr(_?1STRINGVAR.16)(P32)
	c0    mov $r0.3 = (_?1STRINGPACKET.16 + 0)   ## addr(_?1STRINGVAR.16)(P32)
	c0    NOP
	c0    NOP
;;								## 0
	c0    add $r0.2 = $r0.1, 0x30   ## bblock 26, line 56,  t113,  t441,  offset(solutions?1.2)=0x30(P32)
	c0    stw 0x14[$r0.1] = $r0.58   ## bblock 26, line 56, t441, t274
	c0    mov $r0.4 = $r0.0   ## 0(I32)
	c0    mov $r0.8 = $r0.0   ## 0(I32)
;;								## 1
	c0    NOP
	c0    mov $r0.3 = (~0x3fdfffff)   ## (~0x3fdfffff)(I32)
	c0    NOP
	c0    mov $r0.5 = (~0x3faf070a)   ## (~0x3faf070a)(I32)
;;								## 2
	c0    NOP
	c0    mov $r0.6 = (~0x3d70a3d6)   ## (~0x3d70a3d6)(I32)
	c0    mov $r0.7 = 1075314688   ## 1075314688(I32)
	c0    NOP
;;								## 3
	c0    NOP
	c0    mov $r0.9 = (~0x3fc86666)   ## (~0x3fc86666)(I32)
	c0    NOP
	c0    mov $r0.10 = (~0x66666665)   ## (~0x66666665)(I32)
;;								## 4
	c0    NOP
	c0    stw 0x10[$r0.1] = $r0.2   ## bblock 26, line 56, t441, t113
	c0    call $l0.0 = SolveCubic   ## bblock 26, line 56,  raddr(SolveCubic)(P32),  (~0x3fdfffff)(I32),  0(I32),  (~0x3faf070a)(I32),  (~0x3d70a3d6)(I32),  1075314688(I32),  0(I32),  (~0x3fc86666)(I32),  (~0x66666665)(I32)
	c0    NOP
;;								## 5
	c0    call $l0.0 = printf   ## bblock 27, line 57,  raddr(printf)(P32),  addr(_?1STRINGVAR.17)(P32)
	c0    NOP
	c0    mov $r0.3 = (_?1STRINGPACKET.17 + 0)   ## addr(_?1STRINGVAR.17)(P32)
	c0    NOP
;;								## 6
	c0    mov $r0.57 = $r0.0   ## bblock 28, line 58,  t486,  0(SI32)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 7
	c0    ldw $r0.2 = 0x30[$r0.1]   ## bblock 29, line 58, t119, t441
	c0    sh3add $r0.7 = $r0.57, $r0.58   ## [spec] bblock 91, line 59,  t561,  t486,  t274
	c0    sh3add $r0.8 = $r0.57, $r0.59   ## [spec] bblock 91, line 59,  t562,  t486,  t275
	c0    mov $r0.4 = $r0.0   ## 0(I32)
;;								## 0
	c0    NOP
	c0    ldw.d $r0.5 = 0[$r0.7]   ## [spec] bblock 91, line 59, t428, t561
	c0    mov $r0.3 = (_?1STRINGPACKET.18 + 0)   ## addr(_?1STRINGVAR.18)(P32)
	c0    NOP
;;								## 1
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw.d $r0.6 = 0[$r0.8]   ## [spec] bblock 91, line 59, t429, t562
;;								## 2
	c0    NOP
	c0    NOP
	c0    NOP
	c0    cmplt $b0.0 = $r0.57, $r0.2   ## bblock 29, line 58,  t543(I1),  t486,  t119
;;								## 4
	c0    NOP
	c0    NOP
	c0    brf $b0.0, L11?3   ## bblock 29, line 58,  t543(I1)
	c0    NOP
;;								## 5
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 91, line 59,  raddr(printf)(P32),  addr(_?1STRINGVAR.18)(P32),  0(I32),  t428,  t429
	c0    NOP
	c0    NOP
;;								## 6
	c0    add $r0.57 = $r0.57, 1   ## bblock 92, line 58,  t486,  t486,  1(SI32)
	c0    goto L10?3 ## goto
	c0    NOP
	c0    NOP
;;								## 7
	c0    call $l0.0 = printf   ## bblock 30, line 60,  raddr(printf)(P32),  addr(_?1STRINGVAR.19)(P32)
	c0    mov $r0.3 = (_?1STRINGPACKET.19 + 0)   ## addr(_?1STRINGVAR.19)(P32)
	c0    NOP
	c0    NOP
;;								## 0
	c0    add $r0.2 = $r0.1, 0x30   ## bblock 31, line 63,  t134,  t441,  offset(solutions?1.2)=0x30(P32)
	c0    mov $r0.4 = $r0.0   ## 0(I32)
	c0    mov $r0.8 = $r0.0   ## 0(I32)
	c0    mov $r0.10 = $r0.0   ## 0(I32)
;;								## 1
	c0    NOP
	c0    mov $r0.3 = 1078362112   ## 1078362112(I32)
	c0    mov $r0.5 = 1075926794   ## 1075926794(I32)
	c0    NOP
;;								## 2
	c0    mov $r0.6 = 1030792151   ## 1030792151(I32)
	c0    NOP
	c0    mov $r0.7 = 1075707904   ## 1075707904(I32)
	c0    NOP
;;								## 3
	c0    stw 0x10[$r0.1] = $r0.2   ## bblock 31, line 63, t441, t134
	c0    NOP
	c0    mov $r0.9 = 1078001664   ## 1078001664(I32)
	c0    NOP
;;								## 4
	c0    stw 0x14[$r0.1] = $r0.58   ## bblock 31, line 63, t441, t274
	c0    NOP
	c0    call $l0.0 = SolveCubic   ## bblock 31, line 63,  raddr(SolveCubic)(P32),  1078362112(I32),  0(I32),  1075926794(I32),  1030792151(I32),  1075707904(I32),  0(I32),  1078001664(I32),  0(I32)
	c0    NOP
;;								## 5
	c0    NOP
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 32, line 64,  raddr(printf)(P32),  addr(_?1STRINGVAR.20)(P32)
	c0    mov $r0.3 = (_?1STRINGPACKET.20 + 0)   ## addr(_?1STRINGVAR.20)(P32)
;;								## 6
	c0    NOP
	c0    mov $r0.57 = $r0.0   ## bblock 33, line 65,  t487,  0(SI32)
	c0    NOP
	c0    NOP
;;								## 7
	c0    ldw $r0.2 = 0x30[$r0.1]   ## bblock 34, line 65, t140, t441
	c0    sh3add $r0.7 = $r0.57, $r0.58   ## [spec] bblock 89, line 66,  t559,  t487,  t274
	c0    sh3add $r0.8 = $r0.57, $r0.59   ## [spec] bblock 89, line 66,  t560,  t487,  t275
	c0    mov $r0.4 = $r0.0   ## 0(I32)
;;								## 0
	c0    ldw.d $r0.5 = 0[$r0.7]   ## [spec] bblock 89, line 66, t426, t559
	c0    NOP
	c0    NOP
	c0    mov $r0.3 = (_?1STRINGPACKET.21 + 0)   ## addr(_?1STRINGVAR.21)(P32)
;;								## 1
	c0    NOP
	c0    NOP
	c0    ldw.d $r0.6 = 0[$r0.8]   ## [spec] bblock 89, line 66, t427, t560
	c0    NOP
;;								## 2
	c0    cmplt $b0.0 = $r0.57, $r0.2   ## bblock 34, line 65,  t544(I1),  t487,  t140
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 4
	c0    brf $b0.0, L13?3   ## bblock 34, line 65,  t544(I1)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 5
	c0    call $l0.0 = printf   ## bblock 89, line 66,  raddr(printf)(P32),  addr(_?1STRINGVAR.21)(P32),  0(I32),  t426,  t427
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 6
	c0    NOP
	c0    add $r0.57 = $r0.57, 1   ## bblock 90, line 65,  t487,  t487,  1(SI32)
	c0    goto L12?3 ## goto
	c0    NOP
;;								## 7
	c0    call $l0.0 = printf   ## bblock 35, line 67,  raddr(printf)(P32),  addr(_?1STRINGVAR.22)(P32)
	c0    NOP
	c0    mov $r0.3 = (_?1STRINGPACKET.22 + 0)   ## addr(_?1STRINGVAR.22)(P32)
	c0    NOP
;;								## 0
	c0    add $r0.2 = $r0.1, 0x30   ## bblock 36, line 70,  t155,  t441,  offset(solutions?1.2)=0x30(P32)
	c0    stw 0x14[$r0.1] = $r0.58   ## bblock 36, line 70, t441, t274
	c0    mov $r0.4 = $r0.0   ## 0(I32)
	c0    mov $r0.10 = $r0.0   ## 0(I32)
;;								## 1
	c0    NOP
	c0    mov $r0.3 = (~0x3fd7ffff)   ## (~0x3fd7ffff)(I32)
	c0    mov $r0.5 = (~0x4004cccc)   ## (~0x4004cccc)(I32)
	c0    NOP
;;								## 2
	c0    mov $r0.6 = 858993459   ## 858993459(I32)
	c0    mov $r0.8 = 858993459   ## 858993459(I32)
	c0    NOP
	c0    NOP
;;								## 3
	c0    NOP
	c0    mov $r0.7 = 1075131187   ## 1075131187(I32)
	c0    NOP
	c0    mov $r0.9 = 1076887552   ## 1076887552(I32)
;;								## 4
	c0    NOP
	c0    stw 0x10[$r0.1] = $r0.2   ## bblock 36, line 70, t441, t155
	c0    call $l0.0 = SolveCubic   ## bblock 36, line 70,  raddr(SolveCubic)(P32),  (~0x3fd7ffff)(I32),  0(I32),  (~0x4004cccc)(I32),  858993459(I32),  1075131187(I32),  858993459(I32),  1076887552(I32),  0(I32)
	c0    NOP
;;								## 5
	c0    call $l0.0 = printf   ## bblock 37, line 71,  raddr(printf)(P32),  addr(_?1STRINGVAR.23)(P32)
	c0    NOP
	c0    mov $r0.3 = (_?1STRINGPACKET.23 + 0)   ## addr(_?1STRINGVAR.23)(P32)
	c0    NOP
;;								## 6
	c0    NOP
	c0    mov $r0.57 = $r0.0   ## bblock 38, line 72,  t488,  0(SI32)
	c0    NOP
	c0    NOP
;;								## 7
	c0    ldw $r0.2 = 0x30[$r0.1]   ## bblock 39, line 72, t161, t441
	c0    sh3add $r0.7 = $r0.57, $r0.58   ## [spec] bblock 87, line 73,  t557,  t488,  t274
	c0    sh3add $r0.8 = $r0.57, $r0.59   ## [spec] bblock 87, line 73,  t558,  t488,  t275
	c0    mov $r0.4 = $r0.0   ## 0(I32)
;;								## 0
	c0    ldw.d $r0.5 = 0[$r0.7]   ## [spec] bblock 87, line 73, t424, t557
	c0    NOP
	c0    mov $r0.3 = (_?1STRINGPACKET.24 + 0)   ## addr(_?1STRINGVAR.24)(P32)
	c0    NOP
;;								## 1
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw.d $r0.6 = 0[$r0.8]   ## [spec] bblock 87, line 73, t425, t558
;;								## 2
	c0    NOP
	c0    cmplt $b0.0 = $r0.57, $r0.2   ## bblock 39, line 72,  t545(I1),  t488,  t161
	c0    NOP
	c0    NOP
;;								## 4
	c0    NOP
	c0    NOP
	c0    brf $b0.0, L15?3   ## bblock 39, line 72,  t545(I1)
	c0    NOP
;;								## 5
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 87, line 73,  raddr(printf)(P32),  addr(_?1STRINGVAR.24)(P32),  0(I32),  t424,  t425
	c0    NOP
	c0    NOP
;;								## 6
	c0    add $r0.57 = $r0.57, 1   ## bblock 88, line 72,  t488,  t488,  1(SI32)
	c0    NOP
	c0    goto L14?3 ## goto
	c0    NOP
;;								## 7
	c0    stw 0x6c[$r0.1] = $r0.60  ## spill ## t447
	c0    NOP
	c0    mov $r0.3 = (_?1STRINGPACKET.25 + 0)   ## addr(_?1STRINGVAR.25)(P32)
	c0    NOP
;;								## 0
	c0    NOP
	c0    stw 0x70[$r0.1] = $r0.61  ## spill ## t448
	c0    NOP
	c0    NOP
;;								## 1
	c0    stw 0x74[$r0.1] = $r0.62  ## spill ## t449
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 2
	c0    stw 0x78[$r0.1] = $r0.63  ## spill ## t450
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 3
	c0    call $l0.0 = printf   ## bblock 40, line 74,  raddr(printf)(P32),  addr(_?1STRINGVAR.25)(P32)
	c0    stw 0x54[$r0.1] = $r0.59  ## spill ## t275
	c0    NOP
	c0    NOP
;;								## 4
	c0    add $r0.59 = $r0.1, 0x30   ## bblock 41, line 81,  t188,  t441,  offset(solutions?1.2)=0x30(P32)
	c0    mov $r0.61 = 1072693248   ## bblock 41, line 77,  t462,  1072693248(I32)
	c0    NOP
	c0    mov $r0.62 = $r0.0   ## bblock 41, line 77,  t463,  0(I32)
;;								## 5
	c0    mov $r0.60 = $r0.58   ## t274
	c0    NOP
	c0    NOP
	c0    stw 0x58[$r0.1] = $r0.59  ## spill ## t188
;;								## 6
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1076101120   ## 1076101120(I32)
	c0    mov $r0.3 = $r0.61   ## t462
	c0    NOP
;;								## 0
	c0    call $l0.0 = _d_lt   ## bblock 42, line 77,  raddr(_d_lt)(P32),  t462,  t463,  1076101120(I32),  0(I32)
	c0    NOP
	c0    NOP
	c0    mov $r0.4 = $r0.62   ## t463
;;								## 1
	c0    cmpne $b0.0 = $r0.3, $r0.0   ## bblock 42, line 77,  t546(I1),  t262,  0(I1)
	c0    mov $r0.63 = 1076101120   ## [spec] bblock 70, line 78,  t460,  1076101120(I32)
	c0    NOP
	c0    mov $r0.59 = $r0.0   ## [spec] bblock 70, line 78,  t461,  0(I32)
;;								## 3
	c0    NOP
	c0    NOP
	c0    brf $b0.0, L17?3   ## bblock 42, line 77,  t546(I1)
	c0    NOP
;;								## 4
	c0    mov $r0.5 = $r0.0   ## 0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.3 = $r0.63   ## t460
	c0    mov $r0.4 = $r0.59   ## t461
;;								## 0
	c0    NOP
	c0    call $l0.0 = _d_gt   ## bblock 71, line 78,  raddr(_d_gt)(P32),  t460,  t461,  0(I32),  0(I32)
	c0    NOP
	c0    NOP
;;								## 1
	c0    cmpne $b0.0 = $r0.3, $r0.0   ## bblock 71, line 78,  t551(I1),  t271,  0(I1)
	c0    NOP
	c0    mov $r0.7 = 1075052544   ## [spec] bblock 73, line 79,  t458,  1075052544(I32)
	c0    mov $r0.8 = $r0.0   ## [spec] bblock 73, line 79,  t459,  0(I32)
;;								## 3
	c0    NOP
	c0    NOP
	c0    brf $b0.0, L19?3   ## bblock 71, line 78,  t551(I1)
	c0    NOP
;;								## 4
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    NOP
	c0    mov $r0.5 = 1076756480   ## 1076756480(I32)
	c0    stw 0x4c[$r0.1] = $r0.7  ## spill ## t458
;;								## 0
	c0    call $l0.0 = _d_lt   ## bblock 74, line 79,  raddr(_d_lt)(P32),  t458,  t459,  1076756480(I32),  0(I32)
	c0    mov $r0.3 = $r0.7   ## t458
	c0    mov $r0.4 = $r0.8   ## t459
	c0    stw 0x50[$r0.1] = $r0.8  ## spill ## t459
;;								## 1
	c0    cmpne $b0.0 = $r0.3, $r0.0   ## bblock 74, line 79,  t552(I1),  t272,  0(I1)
	c0    mov $r0.57 = (~0x400fffff)   ## [spec] bblock 76, line 80,  t456,  (~0x400fffff)(I32)
	c0    NOP
	c0    mov $r0.58 = $r0.0   ## [spec] bblock 76, line 80,  t457,  0(I32)
;;								## 2
	c0    ldw $r0.7 = 0x4c[$r0.1]  ## restore ## t458
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 3
	c0    ldw $r0.8 = 0x50[$r0.1]  ## restore ## t459
	c0    mov $r0.2 = $r0.59   ## t461
	c0    NOP
	c0    brf $b0.0, L21?3   ## bblock 74, line 79,  t552(I1)
;;								## 4
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.59 = 0x58[$r0.1]  ## restore ## t188
;;								## 6
	c0    NOP
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 7
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    NOP
	c0    mov $r0.5 = (~0x3febffff)   ## (~0x3febffff)(I32)
	c0    mov $r0.3 = $r0.57   ## t456
;;								## 0
	c0    mov $r0.4 = $r0.58   ## t457
	c0    stw 0x40[$r0.1] = $r0.62  ## spill ## t463
	c0    NOP
	c0    NOP
;;								## 1
	c0    stw 0x44[$r0.1] = $r0.63  ## spill ## t460
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 2
	c0    NOP
	c0    stw 0x48[$r0.1] = $r0.2  ## spill ## t461
	c0    NOP
	c0    NOP
;;								## 3
	c0    NOP
	c0    stw 0x4c[$r0.1] = $r0.7  ## spill ## t458
	c0    NOP
	c0    NOP
;;								## 4
	c0    call $l0.0 = _d_gt   ## bblock 77, line 80,  raddr(_d_gt)(P32),  t456,  t457,  (~0x3febffff)(I32),  0(I32)
	c0    NOP
	c0    NOP
	c0    stw 0x50[$r0.1] = $r0.8  ## spill ## t459
;;								## 5
	c0    cmpne $b0.0 = $r0.3, $r0.0   ## bblock 77, line 80,  t553(I1),  t273,  0(I1)
	c0    mov $r0.9 = $r0.57   ## t456
	c0    mov $r0.10 = $r0.58   ## t457
	c0    ldw $r0.6 = 0x48[$r0.1]  ## restore ## t461
;;								## 6
	c0    mov $r0.3 = $r0.61   ## t462
	c0    mov $r0.4 = $r0.62   ## t463
	c0    mov $r0.5 = $r0.63   ## t460
	c0    ldw $r0.7 = 0x4c[$r0.1]  ## restore ## t458
;;								## 7
	c0    ldw $r0.8 = 0x50[$r0.1]  ## restore ## t459
	c0    NOP
	c0    NOP
	c0    brf $b0.0, L23?3   ## bblock 77, line 80,  t553(I1)
;;								## 8
	c0    NOP
	c0    stw 0x10[$r0.1] = $r0.59   ## bblock 79, line 81, t441, t188
	c0    NOP
	c0    NOP
;;								## 9
	c0    NOP
	c0    stw 0x14[$r0.1] = $r0.60   ## bblock 79, line 81, t441, t274
	c0    NOP
	c0    call $l0.0 = SolveCubic   ## bblock 79, line 81,  raddr(SolveCubic)(P32),  t462,  t463,  t460,  t461,  t458,  t459,  t456,  t457
;;								## 10
	c0    NOP
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 80, line 82,  raddr(printf)(P32),  addr(_?1STRINGVAR.26)(P32)
	c0    mov $r0.3 = (_?1STRINGPACKET.26 + 0)   ## addr(_?1STRINGVAR.26)(P32)
;;								## 11
	c0    mov $r0.57 = $r0.0   ## bblock 81, line 83,  t492,  0(SI32)
	c0    mov $r0.62 = $r0.59   ## t188
	c0    mov $r0.63 = $r0.61   ## t462
	c0    mov $r0.2 = $r0.57   ## t456
;;								## 12
	c0    mov $r0.61 = $r0.58   ## t457
	c0    mov $r0.58 = $r0.60   ## t274
	c0    mov $r0.60 = $r0.2   ## t456
	c0    ldw $r0.59 = 0x54[$r0.1]  ## restore ## t275
;;								## 14
	c0    NOP
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 15
	c0    ldw $r0.2 = 0x30[$r0.1]   ## bblock 82, line 83, t194, t441
	c0    sh3add $r0.7 = $r0.57, $r0.58   ## [spec] bblock 85, line 84,  t555,  t492,  t274
	c0    sh3add $r0.8 = $r0.57, $r0.59   ## [spec] bblock 85, line 84,  t556,  t492,  t275
	c0    mov $r0.4 = $r0.0   ## 0(I32)
;;								## 0
	c0    NOP
	c0    ldw.d $r0.5 = 0[$r0.7]   ## [spec] bblock 85, line 84, t422, t555
	c0    mov $r0.3 = (_?1STRINGPACKET.27 + 0)   ## addr(_?1STRINGVAR.27)(P32)
	c0    NOP
;;								## 1
	c0    NOP
	c0    ldw.d $r0.6 = 0[$r0.8]   ## [spec] bblock 85, line 84, t423, t556
	c0    NOP
	c0    NOP
;;								## 2
	c0    cmplt $b0.0 = $r0.57, $r0.2   ## bblock 82, line 83,  t554(I1),  t492,  t194
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 4
	c0    NOP
	c0    brf $b0.0, L25?3   ## bblock 82, line 83,  t554(I1)
	c0    NOP
	c0    NOP
;;								## 5
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 85, line 84,  raddr(printf)(P32),  addr(_?1STRINGVAR.27)(P32),  0(I32),  t422,  t423
	c0    NOP
	c0    NOP
;;								## 6
	c0    NOP
	c0    add $r0.57 = $r0.57, 1   ## bblock 86, line 83,  t492,  t492,  1(SI32)
	c0    goto L24?3 ## goto
	c0    NOP
;;								## 7
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 83, line 85,  raddr(printf)(P32),  addr(_?1STRINGVAR.28)(P32)
	c0    stw 0x54[$r0.1] = $r0.59  ## spill ## t275
	c0    mov $r0.3 = (_?1STRINGPACKET.28 + 0)   ## addr(_?1STRINGVAR.28)(P32)
;;								## 0
	c0    NOP
	c0    mov $r0.6 = $r0.61   ## t457
	c0    mov $r0.5 = $r0.60   ## t456
	c0    mov $r0.3 = (~0x402322d0)   ## (~0x402322d0)(I32)
;;								## 1
	c0    NOP
	c0    call $l0.0 = _d_add   ## bblock 84, line 80,  raddr(_d_add)(P32),  (~0x402322d0)(I32),  446676599(I32),  t456,  t457
	c0    mov $r0.4 = 446676599   ## 446676599(I32)
	c0    NOP
;;								## 2
	c0    mov $r0.59 = $r0.62   ## t188
	c0    mov $r0.60 = $r0.58   ## t274
	c0    ldw $r0.7 = 0x4c[$r0.1]  ## restore ## t458
	c0    mov $r0.61 = $r0.63   ## t462
;;								## 3
	c0    ldw $r0.8 = 0x50[$r0.1]  ## restore ## t459
	c0    mov $r0.57 = $r0.3   ## t456
	c0    NOP
	c0    mov $r0.58 = $r0.4   ## t457
;;								## 4
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.63 = 0x44[$r0.1]  ## restore ## t460
;;								## 5
	c0    NOP
	c0    NOP
	c0    ldw $r0.2 = 0x48[$r0.1]  ## restore ## t461
	c0    NOP
;;								## 6
	c0    NOP
	c0    ldw $r0.62 = 0x40[$r0.1]  ## restore ## t463
	c0    NOP
	c0    NOP
;;								## 8
	c0    goto L22?3 ## goto
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 9
	c0    stw 0x58[$r0.1] = $r0.59  ## spill ## t188
	c0    NOP
	c0    mov $r0.3 = 1071875358   ## 1071875358(I32)
	c0    NOP
;;								## 0
	c0    NOP
	c0    NOP
	c0    stw 0x44[$r0.1] = $r0.5  ## spill ## t460
	c0    NOP
;;								## 1
	c0    stw 0x48[$r0.1] = $r0.6  ## spill ## t461
	c0    mov $r0.5 = $r0.7   ## t458
	c0    NOP
	c0    NOP
;;								## 2
	c0    stw 0x40[$r0.1] = $r0.4  ## spill ## t463
	c0    NOP
	c0    mov $r0.6 = $r0.8   ## t459
	c0    NOP
;;								## 3
	c0    NOP
	c0    call $l0.0 = _d_add   ## bblock 78, line 79,  raddr(_d_add)(P32),  1071875358(I32),  (~0x47ae147a)(I32),  t458,  t459
	c0    NOP
	c0    mov $r0.4 = (~0x47ae147a)   ## (~0x47ae147a)(I32)
;;								## 4
	c0    ldw $r0.63 = 0x44[$r0.1]  ## restore ## t460
	c0    NOP
	c0    mov $r0.7 = $r0.3   ## t458
	c0    mov $r0.8 = $r0.4   ## t459
;;								## 5
	c0    NOP
	c0    NOP
	c0    ldw $r0.59 = 0x48[$r0.1]  ## restore ## t461
	c0    NOP
;;								## 6
	c0    NOP
	c0    ldw $r0.62 = 0x40[$r0.1]  ## restore ## t463
	c0    NOP
	c0    NOP
;;								## 8
	c0    goto L20?3 ## goto
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 9
	c0    mov $r0.6 = $r0.2   ## t461
	c0    NOP
	c0    mov $r0.5 = $r0.63   ## t460
	c0    mov $r0.4 = $r0.0   ## 0(I32)
;;								## 0
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_add   ## bblock 75, line 78,  raddr(_d_add)(P32),  (~0x402fffff)(I32),  0(I32),  t460,  t461
	c0    mov $r0.3 = (~0x402fffff)   ## (~0x402fffff)(I32)
;;								## 1
	c0    mov $r0.63 = $r0.3   ## t460
	c0    mov $r0.59 = $r0.4   ## t461
	c0    NOP
	c0    goto L18?3 ## goto
;;								## 2
	c0    mov $r0.6 = $r0.62   ## t463
	c0    mov $r0.5 = $r0.61   ## t462
	c0    NOP
	c0    mov $r0.4 = $r0.0   ## 0(I32)
;;								## 0
	c0    call $l0.0 = _d_add   ## bblock 72, line 77,  raddr(_d_add)(P32),  1072693248(I32),  0(I32),  t462,  t463
	c0    mov $r0.3 = 1072693248   ## 1072693248(I32)
	c0    NOP
	c0    NOP
;;								## 1
	c0    NOP
	c0    mov $r0.61 = $r0.3   ## t462
	c0    mov $r0.62 = $r0.4   ## t463
	c0    goto L16?3 ## goto
;;								## 2
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 43, line 92,  raddr(printf)(P32),  addr(_?1STRINGVAR.29)(P32)
	c0    mov $r0.3 = (_?1STRINGPACKET.29 + 0)   ## addr(_?1STRINGVAR.29)(P32)
	c0    NOP
;;								## 0
	c0    NOP
	c0    add $r0.58 = $r0.1, 0x34   ## bblock 44, line 96,  t212,  t441,  offset(q?1.2)=0x34(P32)
	c0    NOP
	c0    mov $r0.57 = $r0.0   ## bblock 44, line 94,  t489,  0(SI32)
;;								## 1
	c0    NOP
	c0    cmplt $b0.0 = $r0.57, 100000   ## bblock 45, line 94,  t547(I1),  t489,  100000(SI32)
	c0    mov $r0.3 = $r0.57   ## t489
	c0    mov $r0.4 = $r0.58   ## t212
;;								## 1
	c0    brf $b0.0, L27?3   ## bblock 45, line 94,  t547(I1)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 2
	c0    call $l0.0 = usqrt   ## bblock 67, line 96,  raddr(usqrt)(P32),  t489,  t212
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 3
	c0    NOP
	c0    ldw $r0.5 = 0x34[$r0.1]   ## bblock 68, line 100, t217, t441
	c0    mov $r0.3 = (_?1STRINGPACKET.30 + 0)   ## addr(_?1STRINGVAR.30)(P32)
	c0    mov $r0.4 = $r0.57   ## t489
;;								## 5
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 68, line 100,  raddr(printf)(P32),  addr(_?1STRINGVAR.30)(P32),  t489,  t217
	c0    NOP
	c0    NOP
;;								## 6
	c0    NOP
	c0    add $r0.57 = $r0.57, 2   ## bblock 69, line 94,  t489,  t489,  2(SI32)
	c0    goto L26?3 ## goto
	c0    NOP
;;								## 7
	c0    call $l0.0 = printf   ## bblock 46, line 102,  raddr(printf)(P32),  addr(_?1STRINGVAR.31)(P32)
	c0    mov $r0.3 = (_?1STRINGPACKET.31 + 0)   ## addr(_?1STRINGVAR.31)(P32)
	c0    NOP
	c0    NOP
;;								## 0
	c0    NOP
	c0    add $r0.58 = $r0.1, 0x34   ## bblock 47, line 105,  t225,  t441,  offset(q?1.2)=0x34(P32)
	c0    mov $r0.57 = 1072497001   ## bblock 47, line 103,  t452,  1072497001(SI32)
	c0    NOP
;;								## 1
	c0    cmpltu $b0.0 = $r0.57, 1072513385   ## bblock 48, line 103,  t548(I1),  t452,  1072513385(SI32)
	c0    mov $r0.3 = $r0.57   ## t452
	c0    mov $r0.4 = $r0.58   ## t225
	c0    NOP
;;								## 1
	c0    brf $b0.0, L29?3   ## bblock 48, line 103,  t548(I1)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 2
	c0    NOP
	c0    NOP
	c0    NOP
	c0    call $l0.0 = usqrt   ## bblock 64, line 105,  raddr(usqrt)(P32),  t452,  t225
;;								## 3
	c0    ldw $r0.5 = 0x34[$r0.1]   ## bblock 65, line 107, t230, t441
	c0    NOP
	c0    mov $r0.3 = (_?1STRINGPACKET.32 + 0)   ## addr(_?1STRINGVAR.32)(P32)
	c0    mov $r0.4 = $r0.57   ## t452
;;								## 5
	c0    NOP
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 65, line 107,  raddr(printf)(P32),  addr(_?1STRINGVAR.32)(P32),  t452,  t230
	c0    NOP
;;								## 6
	c0    add $r0.57 = $r0.57, 1   ## bblock 66, line 103,  t452,  t452,  1(SI32)
	c0    NOP
	c0    NOP
	c0    goto L28?3 ## goto
;;								## 7
	c0    NOP
	c0    call $l0.0 = printf   ## bblock 49, line 111,  raddr(printf)(P32),  addr(_?1STRINGVAR.33)(P32)
	c0    NOP
	c0    mov $r0.3 = (_?1STRINGPACKET.33 + 0)   ## addr(_?1STRINGVAR.33)(P32)
;;								## 0
	c0    NOP
	c0    mov $r0.57 = $r0.0   ## bblock 50, line 114,  t454,  0(I32)
	c0    mov $r0.58 = $r0.0   ## bblock 50, line 114,  t455,  0(I32)
	c0    NOP
;;								## 1
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1081507840   ## 1081507840(I32)
	c0    NOP
	c0    mov $r0.3 = $r0.57   ## t454
;;								## 0
	c0    call $l0.0 = _d_le   ## bblock 51, line 114,  raddr(_d_le)(P32),  t454,  t455,  1081507840(I32),  0(I32)
	c0    mov $r0.4 = $r0.58   ## t455
	c0    NOP
	c0    NOP
;;								## 1
	c0    NOP
	c0    cmpne $b0.0 = $r0.3, $r0.0   ## bblock 51, line 114,  t549(I1),  t263,  0(I1)
	c0    mov $r0.4 = $r0.0   ## 0(I32)
	c0    mov $r0.3 = 1072693248   ## 1072693248(I32)
;;								## 3
	c0    NOP
	c0    NOP
	c0    NOP
	c0    brf $b0.0, L31?3   ## bblock 51, line 114,  t549(I1)
;;								## 4
	c0    call $l0.0 = atan   ## bblock 61, line 115,  raddr(atan)(P32),  1072693248(I32),  0(I32)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 5
	c0    call $l0.0 = _d_div   ## bblock 62, line 115,  raddr(_d_div)(P32),  t378,  t379,  1080459264(I32),  0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    NOP
	c0    mov $r0.5 = 1080459264   ## 1080459264(I32)
;;								## 6
	c0    NOP
	c0    call $l0.0 = _d_mul   ## bblock 62, line 115,  raddr(_d_mul)(P32),  t382,  t383,  1074790400(I32),  0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1074790400   ## 1074790400(I32)
;;								## 7
	c0    call $l0.0 = _d_mul   ## bblock 62, line 115,  raddr(_d_mul)(P32),  t384,  t385,  t454,  t455
	c0    mov $r0.5 = $r0.57   ## t454
	c0    mov $r0.6 = $r0.58   ## t455
	c0    NOP
;;								## 8
	c0    mov $r0.5 = $r0.57   ## t454
	c0    mov $r0.6 = $r0.58   ## t455
	c0    mov $r0.7 = $r0.3   ## t386
	c0    mov $r0.8 = $r0.4   ## t387
;;								## 9
	c0    call $l0.0 = printf   ## bblock 62, line 115,  raddr(printf)(P32),  addr(_?1STRINGVAR.34)(P32),  0(I32),  t454,  t455,  t386,  t387
	c0    mov $r0.3 = (_?1STRINGPACKET.34 + 0)   ## addr(_?1STRINGVAR.34)(P32)
	c0    mov $r0.4 = $r0.0   ## 0(I32)
	c0    NOP
;;								## 10
	c0    mov $r0.3 = 1062232653   ## 1062232653(I32)
	c0    NOP
	c0    NOP
	c0    mov $r0.4 = (~0x2d0e5603)   ## (~0x2d0e5603)(I32)
;;								## 11
	c0    call $l0.0 = _d_add   ## bblock 63, line 114,  raddr(_d_add)(P32),  1062232653(I32),  (~0x2d0e5603)(I32),  t454,  t455
	c0    mov $r0.5 = $r0.57   ## t454
	c0    mov $r0.6 = $r0.58   ## t455
	c0    NOP
;;								## 12
	c0    mov $r0.57 = $r0.3   ## t454
	c0    mov $r0.58 = $r0.4   ## t455
	c0    NOP
	c0    goto L30?3 ## goto
;;								## 13
	c0    call $l0.0 = puts   ## bblock 52, line 116,  raddr(puts)(P32),  addr(_?1STRINGVAR.35)(P32)
	c0    NOP
	c0    mov $r0.3 = (_?1STRINGPACKET.35 + 0)   ## addr(_?1STRINGVAR.35)(P32)
	c0    NOP
;;								## 0
	c0    mov $r0.58 = $r0.0   ## bblock 53, line 118,  t490,  0(I32)
	c0    mov $r0.59 = $r0.0   ## bblock 53, line 118,  t491,  0(I32)
	c0    mov $r0.4 = $r0.0   ## 0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
;;								## 1
	c0    NOP
	c0    NOP
	c0    mov $r0.3 = 1080459264   ## 1080459264(I32)
	c0    mov $r0.5 = 1074790400   ## 1074790400(I32)
;;								## 2
	c0    call $l0.0 = _d_div   ## bblock 53, line 119,  raddr(_d_div)(P32),  1080459264(I32),  0(I32),  1074790400(I32),  0(I32)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 3
	c0    NOP
	c0    mov $r0.57 = 1072693248   ## 1072693248(I32)
	c0    mov $r0.60 = $r0.3   ## t360
	c0    mov $r0.61 = $r0.4   ## t361
;;								## 4
	c0    call $l0.0 = atan   ## bblock 54, line 118,  raddr(atan)(P32),  1072693248(I32),  0(I32)
	c0    NOP
	c0    mov $r0.4 = $r0.0   ## 0(I32)
	c0    mov $r0.3 = $r0.57   ## 1072693248(I32)
;;								## 0
	c0    call $l0.0 = _d_mul   ## bblock 55, line 118,  raddr(_d_mul)(P32),  t346,  t347,  1074790400(I32),  0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    NOP
	c0    mov $r0.5 = 1074790400   ## 1074790400(I32)
;;								## 1
	c0    call $l0.0 = _d_mul   ## bblock 55, line 118,  raddr(_d_mul)(P32),  t348,  t349,  1073741824(I32),  0(I32)
	c0    NOP
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1073741824   ## 1073741824(I32)
;;								## 2
	c0    mov $r0.3 = 1051772663   ## 1051772663(I32)
	c0    mov $r0.5 = $r0.3   ## t350
	c0    mov $r0.6 = $r0.4   ## t351
	c0    NOP
;;								## 3
	c0    call $l0.0 = _d_add   ## bblock 55, line 118,  raddr(_d_add)(P32),  1051772663(I32),  (~0x5f4a1272)(I32),  t350,  t351
	c0    NOP
	c0    NOP
	c0    mov $r0.4 = (~0x5f4a1272)   ## (~0x5f4a1272)(I32)
;;								## 4
	c0    mov $r0.3 = $r0.58   ## t490
	c0    mov $r0.4 = $r0.59   ## t491
	c0    mov $r0.5 = $r0.3   ## t352
	c0    mov $r0.6 = $r0.4   ## t353
;;								## 5
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_le   ## bblock 55, line 118,  raddr(_d_le)(P32),  t490,  t491,  t352,  t353
	c0    NOP
;;								## 6
	c0    NOP
	c0    cmpne $b0.0 = $r0.3, $r0.0   ## bblock 55, line 118,  t550(I1),  t265,  0(I1)
	c0    mov $r0.4 = $r0.0   ## 0(I32)
	c0    mov $r0.3 = $r0.57   ## 1072693248(I32)
;;								## 8
	c0    NOP
	c0    NOP
	c0    NOP
	c0    brf $b0.0, L33?3   ## bblock 55, line 118,  t550(I1)
;;								## 9
	c0    call $l0.0 = atan   ## bblock 57, line 119,  raddr(atan)(P32),  1072693248(I32),  0(I32)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 10
	c0    mov $r0.3 = $r0.60   ## t360
	c0    mov $r0.4 = $r0.61   ## t361
	c0    mov $r0.5 = $r0.3   ## t356
	c0    mov $r0.6 = $r0.4   ## t357
;;								## 11
	c0    NOP
	c0    call $l0.0 = _d_div   ## bblock 58, line 119,  raddr(_d_div)(P32),  t360,  t361,  t356,  t357
	c0    NOP
	c0    NOP
;;								## 12
	c0    call $l0.0 = _d_mul   ## bblock 58, line 119,  raddr(_d_mul)(P32),  t362,  t363,  t490,  t491
	c0    mov $r0.5 = $r0.58   ## t490
	c0    NOP
	c0    mov $r0.6 = $r0.59   ## t491
;;								## 13
	c0    mov $r0.5 = $r0.58   ## t490
	c0    mov $r0.6 = $r0.59   ## t491
	c0    mov $r0.7 = $r0.3   ## t364
	c0    mov $r0.8 = $r0.4   ## t365
;;								## 14
	c0    call $l0.0 = printf   ## bblock 58, line 119,  raddr(printf)(P32),  addr(_?1STRINGVAR.36)(P32),  0(I32),  t490,  t491,  t364,  t365
	c0    mov $r0.3 = (_?1STRINGPACKET.36 + 0)   ## addr(_?1STRINGVAR.36)(P32)
	c0    NOP
	c0    mov $r0.4 = $r0.0   ## 0(I32)
;;								## 15
	c0    call $l0.0 = atan   ## bblock 59, line 118,  raddr(atan)(P32),  1072693248(I32),  0(I32)
	c0    mov $r0.4 = $r0.0   ## 0(I32)
	c0    NOP
	c0    mov $r0.3 = $r0.57   ## 1072693248(I32)
;;								## 16
	c0    call $l0.0 = _d_div   ## bblock 60, line 118,  raddr(_d_div)(P32),  t368,  t369,  1085702144(I32),  0(I32)
	c0    NOP
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1085702144   ## 1085702144(I32)
;;								## 17
	c0    call $l0.0 = _d_mul   ## bblock 60, line 118,  raddr(_d_mul)(P32),  t370,  t371,  1074790400(I32),  0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    NOP
	c0    mov $r0.5 = 1074790400   ## 1074790400(I32)
;;								## 18
	c0    call $l0.0 = _d_add   ## bblock 60, line 118,  raddr(_d_add)(P32),  t372,  t373,  t490,  t491
	c0    mov $r0.5 = $r0.58   ## t490
	c0    mov $r0.6 = $r0.59   ## t491
	c0    NOP
;;								## 19
	c0    mov $r0.58 = $r0.3   ## t490
	c0    mov $r0.59 = $r0.4   ## t491
	c0    NOP
	c0    goto L32?3 ## goto
;;								## 20
	c0    mov $r0.3 = $r0.0   ## 0(SI32)
	c0    ldw $l0.0 = 0x3c[$r0.1]  ## restore ## t440
	c0    NOP
	c0    NOP
;;								## 0
	c0    ldw $r0.63 = 0x78[$r0.1]  ## restore ## t450
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 1
	c0    NOP
	c0    NOP
	c0    ldw $r0.62 = 0x74[$r0.1]  ## restore ## t449
	c0    NOP
;;								## 2
	c0    ldw $r0.61 = 0x70[$r0.1]  ## restore ## t448
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 3
	c0    NOP
	c0    ldw $r0.60 = 0x6c[$r0.1]  ## restore ## t447
	c0    NOP
	c0    NOP
;;								## 4
	c0    NOP
	c0    ldw $r0.59 = 0x64[$r0.1]  ## restore ## t446
	c0    NOP
	c0    NOP
;;								## 5
	c0    NOP
	c0    ldw $r0.58 = 0x60[$r0.1]  ## restore ## t445
	c0    NOP
	c0    NOP
;;								## 6
	c0    NOP
	c0    NOP
	c0    ldw $r0.57 = 0x5c[$r0.1]  ## restore ## t444
	c0    NOP
;;								## 8
	c0    NOP
	c0    NOP
	c0    return $r0.1 = $r0.1, (0x80), $l0.0   ## bblock 56, line 122,  t441,  ?2.1?2auto_size(I32),  t440
	c0    NOP
;;								## 9
	c0    NOP
	c0    add $r0.1 = $r0.1, (-0x100)
	c0    NOP
	c0    NOP
;;								## 0
	c0    NOP
	c0    NOP
	c0    stw 0x10[$r0.1] = $l0.0  ## spill ## t342
	c0    NOP
;;								## 1
	c0    NOP
	c0    NOP
	c0    stw 0x14[$r0.1] = $r0.10  ## spill ## t143
	c0    NOP
;;								## 2
	c0    stw 0x18[$r0.1] = $r0.9  ## spill ## t142
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 3
	c0    stw 0x1c[$r0.1] = $r0.8  ## spill ## t137
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 4
	c0    NOP
	c0    NOP
	c0    stw 0x20[$r0.1] = $r0.7  ## spill ## t136
	c0    NOP
;;								## 5
	c0    stw 0x24[$r0.1] = $r0.4  ## spill ## t141
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 6
	c0    NOP
	c0    stw 0x28[$r0.1] = $r0.3  ## spill ## t140
	c0    mov $r0.4 = $r0.6   ## t131
	c0    NOP
;;								## 7
	c0    ldw $r0.2 = 0x114[$r0.1]   ## bblock 0, line 12, t370, t343
	c0    mov $r0.3 = $r0.5   ## t130
	c0    NOP
	c0    NOP
;;								## 8
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.7 = 0x110[$r0.1]   ## bblock 0, line 12, t369, t343
;;								## 9
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.6 = 0x24[$r0.1]  ## restore ## t141
;;								## 10
	c0    NOP
	c0    stw 0x2c[$r0.1] = $r0.2  ## spill ## t370
	c0    NOP
	c0    NOP
;;								## 11
	c0    stw 0x30[$r0.1] = $r0.7  ## spill ## t369
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 12
	c0    NOP
	c0    NOP
	c0    ldw $r0.5 = 0x28[$r0.1]  ## restore ## t140
	c0    NOP
;;								## 14
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_div   ## bblock 0, line 19,  raddr(_d_div)(P32),  t130,  t131,  t140,  t141
	c0    NOP
;;								## 15
	c0    stw 0x34[$r0.1] = $r0.3  ## spill ## t383
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 16
	c0    NOP
	c0    NOP
	c0    stw 0x38[$r0.1] = $r0.4  ## spill ## t384
	c0    NOP
;;								## 17
	c0    NOP
	c0    ldw $r0.4 = 0x1c[$r0.1]  ## restore ## t137
	c0    NOP
	c0    NOP
;;								## 18
	c0    NOP
	c0    NOP
	c0    ldw $r0.3 = 0x20[$r0.1]  ## restore ## t136
	c0    NOP
;;								## 19
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.6 = 0x24[$r0.1]  ## restore ## t141
;;								## 20
	c0    NOP
	c0    ldw $r0.5 = 0x28[$r0.1]  ## restore ## t140
	c0    NOP
	c0    NOP
;;								## 22
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_div   ## bblock 0, line 19,  raddr(_d_div)(P32),  t136,  t137,  t140,  t141
	c0    NOP
;;								## 23
	c0    NOP
	c0    stw 0x3c[$r0.1] = $r0.3  ## spill ## t164
	c0    NOP
	c0    NOP
;;								## 24
	c0    NOP
	c0    stw 0x40[$r0.1] = $r0.4  ## spill ## t165
	c0    NOP
	c0    NOP
;;								## 25
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0x14[$r0.1]  ## restore ## t143
;;								## 26
	c0    NOP
	c0    NOP
	c0    ldw $r0.3 = 0x18[$r0.1]  ## restore ## t142
	c0    NOP
;;								## 27
	c0    NOP
	c0    NOP
	c0    ldw $r0.6 = 0x24[$r0.1]  ## restore ## t141
	c0    NOP
;;								## 28
	c0    NOP
	c0    ldw $r0.5 = 0x28[$r0.1]  ## restore ## t140
	c0    NOP
	c0    NOP
;;								## 30
	c0    NOP
	c0    call $l0.0 = _d_div   ## bblock 0, line 19,  raddr(_d_div)(P32),  t142,  t143,  t140,  t141
	c0    NOP
	c0    NOP
;;								## 31
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1074266112   ## 1074266112(I32)
	c0    NOP
	c0    stw 0x44[$r0.1] = $r0.3  ## spill ## t160
;;								## 32
	c0    stw 0x48[$r0.1] = $r0.4  ## spill ## t161
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 33
	c0    NOP
	c0    NOP
	c0    ldw $r0.3 = 0x3c[$r0.1]  ## restore ## t164
	c0    NOP
;;								## 34
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0x40[$r0.1]  ## restore ## t165
;;								## 36
	c0    NOP
	c0    call $l0.0 = _d_mul   ## bblock 0, line 20,  raddr(_d_mul)(P32),  t164,  t165,  1074266112(I32),  0(I32)
	c0    NOP
	c0    NOP
;;								## 37
	c0    stw 0x4c[$r0.1] = $r0.3  ## spill ## t148
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 38
	c0    NOP
	c0    NOP
	c0    stw 0x50[$r0.1] = $r0.4  ## spill ## t149
	c0    NOP
;;								## 39
	c0    ldw $r0.5 = 0x34[$r0.1]  ## restore ## t383
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 40
	c0    NOP
	c0    ldw $r0.6 = 0x38[$r0.1]  ## restore ## t384
	c0    NOP
	c0    NOP
;;								## 42
	c0    NOP
	c0    NOP
	c0    mov $r0.3 = $r0.5   ## t383
	c0    NOP
;;								## 43
	c0    call $l0.0 = _d_mul   ## bblock 0, line 20,  raddr(_d_mul)(P32),  t383,  t384,  t383,  t384
	c0    NOP
	c0    mov $r0.4 = $r0.6   ## t384
	c0    NOP
;;								## 44
	c0    NOP
	c0    NOP
	c0    NOP
	c0    stw 0x54[$r0.1] = $r0.3  ## spill ## t178
;;								## 45
	c0    stw 0x58[$r0.1] = $r0.4  ## spill ## t179
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 46
	c0    NOP
	c0    ldw $r0.5 = 0x34[$r0.1]  ## restore ## t383
	c0    NOP
	c0    NOP
;;								## 47
	c0    NOP
	c0    ldw $r0.6 = 0x38[$r0.1]  ## restore ## t384
	c0    NOP
	c0    NOP
;;								## 49
	c0    call $l0.0 = _d_mul   ## bblock 0, line 21,  raddr(_d_mul)(P32),  t178,  t179,  t383,  t384
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 50
	c0    NOP
	c0    NOP
	c0    NOP
	c0    stw 0x5c[$r0.1] = $r0.3  ## spill ## t180
;;								## 51
	c0    NOP
	c0    NOP
	c0    NOP
	c0    stw 0x60[$r0.1] = $r0.4  ## spill ## t181
;;								## 52
	c0    NOP
	c0    ldw $r0.5 = 0x34[$r0.1]  ## restore ## t383
	c0    NOP
	c0    NOP
;;								## 53
	c0    ldw $r0.6 = 0x38[$r0.1]  ## restore ## t384
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 54
	c0    NOP
	c0    NOP
	c0    ldw $r0.3 = 0x3c[$r0.1]  ## restore ## t164
	c0    NOP
;;								## 55
	c0    ldw $r0.4 = 0x40[$r0.1]  ## restore ## t165
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 57
	c0    NOP
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_mul   ## bblock 0, line 21,  raddr(_d_mul)(P32),  t164,  t165,  t383,  t384
;;								## 58
	c0    NOP
	c0    NOP
	c0    NOP
	c0    stw 0x64[$r0.1] = $r0.3  ## spill ## t168
;;								## 59
	c0    NOP
	c0    stw 0x68[$r0.1] = $r0.4  ## spill ## t169
	c0    NOP
	c0    NOP
;;								## 60
	c0    NOP
	c0    NOP
	c0    ldw $r0.5 = 0x4c[$r0.1]  ## restore ## t148
	c0    NOP
;;								## 61
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.6 = 0x50[$r0.1]  ## restore ## t149
;;								## 62
	c0    ldw $r0.3 = 0x54[$r0.1]  ## restore ## t178
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 63
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0x58[$r0.1]  ## restore ## t179
	c0    NOP
;;								## 65
	c0    NOP
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_sub   ## bblock 0, line 20,  raddr(_d_sub)(P32),  t178,  t179,  t148,  t149
;;								## 66
	c0    NOP
	c0    call $l0.0 = _d_div   ## bblock 0, line 20,  raddr(_d_div)(P32),  t156,  t157,  1075970048(I32),  0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1075970048   ## 1075970048(I32)
;;								## 67
	c0    stw 0x6c[$r0.1] = $r0.3  ## spill ## t377
	c0    NOP
	c0    mov $r0.5 = $r0.3   ## t377
	c0    mov $r0.6 = $r0.4   ## t378
;;								## 68
	c0    call $l0.0 = _d_mul   ## bblock 0, line 22,  raddr(_d_mul)(P32),  t377,  t378,  t377,  t378
	c0    NOP
	c0    NOP
	c0    stw 0x70[$r0.1] = $r0.4  ## spill ## t378
;;								## 69
	c0    NOP
	c0    ldw $r0.5 = 0x6c[$r0.1]  ## restore ## t377
	c0    NOP
	c0    NOP
;;								## 70
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.6 = 0x70[$r0.1]  ## restore ## t378
;;								## 72
	c0    NOP
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_mul   ## bblock 0, line 22,  raddr(_d_mul)(P32),  t196,  t197,  t377,  t378
;;								## 73
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1075970048   ## 1075970048(I32)
	c0    NOP
	c0    stw 0x74[$r0.1] = $r0.3  ## spill ## t198
;;								## 74
	c0    NOP
	c0    stw 0x78[$r0.1] = $r0.4  ## spill ## t199
	c0    NOP
	c0    NOP
;;								## 75
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.3 = 0x64[$r0.1]  ## restore ## t168
;;								## 76
	c0    ldw $r0.4 = 0x68[$r0.1]  ## restore ## t169
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 78
	c0    call $l0.0 = _d_mul   ## bblock 0, line 21,  raddr(_d_mul)(P32),  t168,  t169,  1075970048(I32),  0(I32)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 79
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1077608448   ## 1077608448(I32)
	c0    stw 0x7c[$r0.1] = $r0.3  ## spill ## t170
	c0    NOP
;;								## 80
	c0    NOP
	c0    stw 0x80[$r0.1] = $r0.4  ## spill ## t171
	c0    NOP
	c0    NOP
;;								## 81
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.3 = 0x44[$r0.1]  ## restore ## t160
;;								## 82
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0x48[$r0.1]  ## restore ## t161
	c0    NOP
;;								## 84
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_mul   ## bblock 0, line 21,  raddr(_d_mul)(P32),  t160,  t161,  1077608448(I32),  0(I32)
	c0    NOP
;;								## 85
	c0    NOP
	c0    NOP
	c0    ldw $r0.5 = 0x7c[$r0.1]  ## restore ## t170
	c0    NOP
;;								## 86
	c0    NOP
	c0    ldw $r0.6 = 0x80[$r0.1]  ## restore ## t171
	c0    NOP
	c0    NOP
;;								## 88
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_sub   ## bblock 0, line 21,  raddr(_d_sub)(P32),  t162,  t163,  t170,  t171
	c0    NOP
;;								## 89
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1073741824   ## 1073741824(I32)
	c0    NOP
	c0    stw 0x84[$r0.1] = $r0.3  ## spill ## t184
;;								## 90
	c0    NOP
	c0    NOP
	c0    NOP
	c0    stw 0x88[$r0.1] = $r0.4  ## spill ## t185
;;								## 91
	c0    NOP
	c0    ldw $r0.3 = 0x5c[$r0.1]  ## restore ## t180
	c0    NOP
	c0    NOP
;;								## 92
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0x60[$r0.1]  ## restore ## t181
;;								## 94
	c0    NOP
	c0    call $l0.0 = _d_mul   ## bblock 0, line 21,  raddr(_d_mul)(P32),  t180,  t181,  1073741824(I32),  0(I32)
	c0    NOP
	c0    NOP
;;								## 95
	c0    mov $r0.5 = $r0.3   ## t182
	c0    NOP
	c0    mov $r0.6 = $r0.4   ## t183
	c0    NOP
;;								## 96
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.3 = 0x84[$r0.1]  ## restore ## t184
;;								## 97
	c0    ldw $r0.4 = 0x88[$r0.1]  ## restore ## t185
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 99
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_add   ## bblock 0, line 21,  raddr(_d_add)(P32),  t184,  t185,  t182,  t183
	c0    NOP
;;								## 100
	c0    call $l0.0 = _d_div   ## bblock 0, line 21,  raddr(_d_div)(P32),  t186,  t187,  1078657024(I32),  0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1078657024   ## 1078657024(I32)
	c0    NOP
;;								## 101
	c0    stw 0x8c[$r0.1] = $r0.3  ## spill ## t375
	c0    mov $r0.5 = $r0.3   ## t375
	c0    mov $r0.6 = $r0.4   ## t376
	c0    NOP
;;								## 102
	c0    NOP
	c0    call $l0.0 = _d_mul   ## bblock 0, line 22,  raddr(_d_mul)(P32),  t375,  t376,  t375,  t376
	c0    NOP
	c0    stw 0x90[$r0.1] = $r0.4  ## spill ## t376
;;								## 103
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.5 = 0x74[$r0.1]  ## restore ## t198
;;								## 104
	c0    ldw $r0.6 = 0x78[$r0.1]  ## restore ## t199
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 106
	c0    NOP
	c0    call $l0.0 = _d_sub   ## bblock 0, line 22,  raddr(_d_sub)(P32),  t204,  t205,  t198,  t199
	c0    NOP
	c0    NOP
;;								## 107
	c0    NOP
	c0    mov $r0.5 = $r0.0   ## 0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    stw 0x94[$r0.1] = $r0.3  ## spill ## t373
;;								## 108
	c0    call $l0.0 = _d_le   ## bblock 0, line 26,  raddr(_d_le)(P32),  t373,  t374,  0(I32),  0(I32)
	c0    NOP
	c0    stw 0x98[$r0.1] = $r0.4  ## spill ## t374
	c0    NOP
;;								## 109
	c0    NOP
	c0    cmpne $b0.0 = $r0.3, $r0.0   ## bblock 0, line 26,  t385(I1),  t120,  0(I1)
	c0    mov $r0.2 = 3   ## 3(SI32)
	c0    ldw $r0.7 = 0x30[$r0.1]  ## restore ## t369
;;								## 110
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0x78[$r0.1]  ## restore ## t199
	c0    NOP
;;								## 111
	c0    NOP
	c0    NOP
	c0    ldw $r0.3 = 0x74[$r0.1]  ## restore ## t198
	c0    brf $b0.0, L0?3   ## bblock 0, line 26,  t385(I1)
;;								## 112
	c0    NOP
	c0    NOP
	c0    NOP
	c0    stw 0[$r0.7] = $r0.2   ## bblock 6, line 28, t369, 3(SI32)
;;								## 113
	c0    NOP
	c0    NOP
	c0    call $l0.0 = sqrt   ## bblock 6, line 29,  raddr(sqrt)(P32),  t198,  t199
	c0    NOP
;;								## 114
	c0    mov $r0.5 = $r0.3   ## t258
	c0    mov $r0.6 = $r0.4   ## t259
	c0    NOP
	c0    NOP
;;								## 115
	c0    NOP
	c0    ldw $r0.3 = 0x8c[$r0.1]  ## restore ## t375
	c0    NOP
	c0    NOP
;;								## 116
	c0    ldw $r0.4 = 0x90[$r0.1]  ## restore ## t376
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 118
	c0    NOP
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_div   ## bblock 7, line 29,  raddr(_d_div)(P32),  t375,  t376,  t258,  t259
;;								## 119
	c0    call $l0.0 = acos   ## bblock 7, line 29,  raddr(acos)(P32),  t262,  t263
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 120
	c0    NOP
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1074266112   ## 1074266112(I32)
	c0    stw 0x9c[$r0.1] = $r0.3  ## spill ## t264
;;								## 121
	c0    NOP
	c0    NOP
	c0    stw 0xa0[$r0.1] = $r0.4  ## spill ## t265
	c0    NOP
;;								## 122
	c0    ldw $r0.3 = 0x34[$r0.1]  ## restore ## t383
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 123
	c0    NOP
	c0    ldw $r0.4 = 0x38[$r0.1]  ## restore ## t384
	c0    NOP
	c0    NOP
;;								## 125
	c0    NOP
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_div   ## bblock 8, line 30,  raddr(_d_div)(P32),  t383,  t384,  1074266112(I32),  0(I32)
;;								## 126
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1074266112   ## 1074266112(I32)
	c0    NOP
	c0    stw 0xa4[$r0.1] = $r0.3  ## spill ## t268
;;								## 127
	c0    stw 0xa8[$r0.1] = $r0.4  ## spill ## t269
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 128
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.3 = 0x9c[$r0.1]  ## restore ## t264
;;								## 129
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0xa0[$r0.1]  ## restore ## t265
;;								## 131
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_div   ## bblock 8, line 30,  raddr(_d_div)(P32),  t264,  t265,  1074266112(I32),  0(I32)
	c0    NOP
;;								## 132
	c0    NOP
	c0    NOP
	c0    NOP
	c0    call $l0.0 = cos   ## bblock 8, line 30,  raddr(cos)(P32),  t272,  t273
;;								## 133
	c0    NOP
	c0    NOP
	c0    NOP
	c0    stw 0xac[$r0.1] = $r0.3  ## spill ## t274
;;								## 134
	c0    NOP
	c0    NOP
	c0    stw 0xb0[$r0.1] = $r0.4  ## spill ## t275
	c0    NOP
;;								## 135
	c0    ldw $r0.3 = 0x6c[$r0.1]  ## restore ## t377
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 136
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0x70[$r0.1]  ## restore ## t378
	c0    NOP
;;								## 138
	c0    call $l0.0 = sqrt   ## bblock 9, line 30,  raddr(sqrt)(P32),  t377,  t378
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 139
	c0    mov $r0.5 = $r0.3   ## t278
	c0    NOP
	c0    NOP
	c0    mov $r0.6 = $r0.4   ## t279
;;								## 140
	c0    ldw $r0.3 = 0xac[$r0.1]  ## restore ## t274
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 141
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0xb0[$r0.1]  ## restore ## t275
	c0    NOP
;;								## 143
	c0    NOP
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_mul   ## bblock 10, line 30,  raddr(_d_mul)(P32),  t274,  t275,  t278,  t279
;;								## 144
	c0    call $l0.0 = _d_mul   ## bblock 10, line 30,  raddr(_d_mul)(P32),  t280,  t281,  (~0x3fffffff)(I32),  0(I32)
	c0    NOP
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = (~0x3fffffff)   ## (~0x3fffffff)(I32)
;;								## 145
	c0    ldw $r0.5 = 0xa4[$r0.1]  ## restore ## t268
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 146
	c0    NOP
	c0    ldw $r0.6 = 0xa8[$r0.1]  ## restore ## t269
	c0    NOP
	c0    NOP
;;								## 148
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_sub   ## bblock 10, line 30,  raddr(_d_sub)(P32),  t282,  t283,  t268,  t269
	c0    NOP
;;								## 149
	c0    NOP
	c0    NOP
	c0    ldw $r0.2 = 0x2c[$r0.1]  ## restore ## t370
	c0    NOP
;;								## 152
	c0    NOP
	c0    NOP
	c0    NOP
	c0    stw 0[$r0.2] = $r0.3   ## bblock 10, line 30, t370, t284
;;								## 153
	c0    NOP
	c0    NOP
	c0    stw 4[$r0.2] = $r0.4   ## bblock 10, line 30, t370, t285
	c0    mov $r0.3 = 1072693248   ## 1072693248(I32)
;;								## 154
	c0    NOP
	c0    call $l0.0 = atan   ## bblock 10, line 31,  raddr(atan)(P32),  1072693248(I32),  0(I32)
	c0    mov $r0.4 = $r0.0   ## 0(I32)
	c0    NOP
;;								## 155
	c0    call $l0.0 = _d_mul   ## bblock 11, line 31,  raddr(_d_mul)(P32),  t290,  t291,  1074790400(I32),  0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    NOP
	c0    mov $r0.5 = 1074790400   ## 1074790400(I32)
;;								## 156
	c0    NOP
	c0    call $l0.0 = _d_mul   ## bblock 11, line 31,  raddr(_d_mul)(P32),  t292,  t293,  1073741824(I32),  0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1073741824   ## 1073741824(I32)
;;								## 157
	c0    NOP
	c0    ldw $r0.5 = 0x9c[$r0.1]  ## restore ## t264
	c0    NOP
	c0    NOP
;;								## 158
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.6 = 0xa0[$r0.1]  ## restore ## t265
;;								## 160
	c0    NOP
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_add   ## bblock 11, line 31,  raddr(_d_add)(P32),  t294,  t295,  t264,  t265
;;								## 161
	c0    call $l0.0 = _d_div   ## bblock 11, line 31,  raddr(_d_div)(P32),  t298,  t299,  1074266112(I32),  0(I32)
	c0    NOP
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1074266112   ## 1074266112(I32)
;;								## 162
	c0    NOP
	c0    NOP
	c0    call $l0.0 = cos   ## bblock 11, line 31,  raddr(cos)(P32),  t300,  t301
	c0    NOP
;;								## 163
	c0    NOP
	c0    NOP
	c0    NOP
	c0    stw 0xb4[$r0.1] = $r0.3  ## spill ## t302
;;								## 164
	c0    NOP
	c0    NOP
	c0    NOP
	c0    stw 0xb8[$r0.1] = $r0.4  ## spill ## t303
;;								## 165
	c0    NOP
	c0    NOP
	c0    ldw $r0.3 = 0x6c[$r0.1]  ## restore ## t377
	c0    NOP
;;								## 166
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0x70[$r0.1]  ## restore ## t378
	c0    NOP
;;								## 168
	c0    call $l0.0 = sqrt   ## bblock 12, line 31,  raddr(sqrt)(P32),  t377,  t378
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 169
	c0    mov $r0.5 = $r0.3   ## t306
	c0    NOP
	c0    NOP
	c0    mov $r0.6 = $r0.4   ## t307
;;								## 170
	c0    NOP
	c0    NOP
	c0    ldw $r0.3 = 0xb4[$r0.1]  ## restore ## t302
	c0    NOP
;;								## 171
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0xb8[$r0.1]  ## restore ## t303
	c0    NOP
;;								## 173
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_mul   ## bblock 13, line 31,  raddr(_d_mul)(P32),  t302,  t303,  t306,  t307
	c0    NOP
;;								## 174
	c0    call $l0.0 = _d_mul   ## bblock 13, line 31,  raddr(_d_mul)(P32),  t308,  t309,  (~0x3fffffff)(I32),  0(I32)
	c0    NOP
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = (~0x3fffffff)   ## (~0x3fffffff)(I32)
;;								## 175
	c0    NOP
	c0    NOP
	c0    ldw $r0.5 = 0xa4[$r0.1]  ## restore ## t268
	c0    NOP
;;								## 176
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.6 = 0xa8[$r0.1]  ## restore ## t269
;;								## 178
	c0    call $l0.0 = _d_sub   ## bblock 13, line 31,  raddr(_d_sub)(P32),  t310,  t311,  t268,  t269
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 179
	c0    NOP
	c0    NOP
	c0    ldw $r0.2 = 0x2c[$r0.1]  ## restore ## t370
	c0    NOP
;;								## 182
	c0    NOP
	c0    NOP
	c0    stw 8[$r0.2] = $r0.3   ## bblock 13, line 31, t370, t312
	c0    NOP
;;								## 183
	c0    NOP
	c0    stw 12[$r0.2] = $r0.4   ## bblock 13, line 31, t370, t313
	c0    NOP
	c0    mov $r0.3 = 1072693248   ## 1072693248(I32)
;;								## 184
	c0    NOP
	c0    call $l0.0 = atan   ## bblock 13, line 32,  raddr(atan)(P32),  1072693248(I32),  0(I32)
	c0    NOP
	c0    mov $r0.4 = $r0.0   ## 0(I32)
;;								## 185
	c0    call $l0.0 = _d_mul   ## bblock 14, line 32,  raddr(_d_mul)(P32),  t318,  t319,  1074790400(I32),  0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    NOP
	c0    mov $r0.5 = 1074790400   ## 1074790400(I32)
;;								## 186
	c0    NOP
	c0    call $l0.0 = _d_mul   ## bblock 14, line 32,  raddr(_d_mul)(P32),  t320,  t321,  1074790400(I32),  0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1074790400   ## 1074790400(I32)
;;								## 187
	c0    NOP
	c0    ldw $r0.5 = 0x9c[$r0.1]  ## restore ## t264
	c0    NOP
	c0    NOP
;;								## 188
	c0    NOP
	c0    ldw $r0.6 = 0xa0[$r0.1]  ## restore ## t265
	c0    NOP
	c0    NOP
;;								## 190
	c0    NOP
	c0    call $l0.0 = _d_add   ## bblock 14, line 32,  raddr(_d_add)(P32),  t322,  t323,  t264,  t265
	c0    NOP
	c0    NOP
;;								## 191
	c0    call $l0.0 = _d_div   ## bblock 14, line 32,  raddr(_d_div)(P32),  t326,  t327,  1074266112(I32),  0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1074266112   ## 1074266112(I32)
	c0    NOP
;;								## 192
	c0    NOP
	c0    NOP
	c0    NOP
	c0    call $l0.0 = cos   ## bblock 14, line 32,  raddr(cos)(P32),  t328,  t329
;;								## 193
	c0    NOP
	c0    NOP
	c0    stw 0xbc[$r0.1] = $r0.3  ## spill ## t330
	c0    NOP
;;								## 194
	c0    NOP
	c0    NOP
	c0    stw 0xc0[$r0.1] = $r0.4  ## spill ## t331
	c0    NOP
;;								## 195
	c0    NOP
	c0    NOP
	c0    ldw $r0.3 = 0x6c[$r0.1]  ## restore ## t377
	c0    NOP
;;								## 196
	c0    ldw $r0.4 = 0x70[$r0.1]  ## restore ## t378
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 198
	c0    NOP
	c0    NOP
	c0    call $l0.0 = sqrt   ## bblock 15, line 32,  raddr(sqrt)(P32),  t377,  t378
	c0    NOP
;;								## 199
	c0    mov $r0.5 = $r0.3   ## t334
	c0    NOP
	c0    mov $r0.6 = $r0.4   ## t335
	c0    NOP
;;								## 200
	c0    NOP
	c0    ldw $r0.3 = 0xbc[$r0.1]  ## restore ## t330
	c0    NOP
	c0    NOP
;;								## 201
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0xc0[$r0.1]  ## restore ## t331
;;								## 203
	c0    NOP
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_mul   ## bblock 16, line 32,  raddr(_d_mul)(P32),  t330,  t331,  t334,  t335
;;								## 204
	c0    call $l0.0 = _d_mul   ## bblock 16, line 32,  raddr(_d_mul)(P32),  t336,  t337,  (~0x3fffffff)(I32),  0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = (~0x3fffffff)   ## (~0x3fffffff)(I32)
	c0    NOP
;;								## 205
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.5 = 0xa4[$r0.1]  ## restore ## t268
;;								## 206
	c0    NOP
	c0    ldw $r0.6 = 0xa8[$r0.1]  ## restore ## t269
	c0    NOP
	c0    NOP
;;								## 208
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_sub   ## bblock 16, line 32,  raddr(_d_sub)(P32),  t338,  t339,  t268,  t269
	c0    NOP
;;								## 209
	c0    NOP
	c0    NOP
	c0    ldw $r0.2 = 0x2c[$r0.1]  ## restore ## t370
	c0    NOP
;;								## 210
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $l0.0 = 0x10[$r0.1]  ## restore ## t342
;;								## 212
	c0    NOP
	c0    stw 16[$r0.2] = $r0.3   ## bblock 16, line 32, t370, t340
	c0    NOP
	c0    NOP
;;								## 213
	c0    NOP
	c0    stw 20[$r0.2] = $r0.4   ## bblock 16, line 32, t370, t341
	c0    NOP
	c0    NOP
;;								## 214
	c0    NOP
	c0    return $r0.1 = $r0.1, (0x100), $l0.0   ## bblock 5, line 42,  t343,  ?2.1?2auto_size(I32),  t342
	c0    NOP
	c0    NOP
;;								## 215
	c0    NOP
	c0    NOP
	c0    mov $r0.2 = 1   ## 1(SI32)
	c0    NOP
;;								## 0
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0x90[$r0.1]  ## restore ## t376
	c0    NOP
;;								## 1
	c0    NOP
	c0    NOP
	c0    ldw $r0.3 = 0x8c[$r0.1]  ## restore ## t375
	c0    NOP
;;								## 2
	c0    NOP
	c0    NOP
	c0    stw 0[$r0.7] = $r0.2   ## bblock 1, line 36, t369, 1(SI32)
	c0    NOP
;;								## 3
	c0    NOP
	c0    NOP
	c0    NOP
	c0    call $l0.0 = fabs   ## bblock 1, line 37,  raddr(fabs)(P32),  t375,  t376
;;								## 4
	c0    NOP
	c0    NOP
	c0    stw 0xc4[$r0.1] = $r0.3  ## spill ## t212
	c0    NOP
;;								## 5
	c0    stw 0xc8[$r0.1] = $r0.4  ## spill ## t213
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 6
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0x98[$r0.1]  ## restore ## t374
	c0    NOP
;;								## 7
	c0    ldw $r0.3 = 0x94[$r0.1]  ## restore ## t373
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 9
	c0    NOP
	c0    NOP
	c0    NOP
	c0    call $l0.0 = sqrt   ## bblock 2, line 37,  raddr(sqrt)(P32),  t373,  t374
;;								## 10
	c0    ldw $r0.3 = 0xc4[$r0.1]  ## restore ## t212
	c0    mov $r0.5 = $r0.3   ## t216
	c0    mov $r0.6 = $r0.4   ## t217
	c0    NOP
;;								## 11
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0xc8[$r0.1]  ## restore ## t213
;;								## 13
	c0    NOP
	c0    call $l0.0 = _d_add   ## bblock 3, line 37,  raddr(_d_add)(P32),  t212,  t213,  t216,  t217
	c0    NOP
	c0    NOP
;;								## 14
	c0    mov $r0.5 = 1070945621   ## 1070945621(I32)
	c0    NOP
	c0    NOP
	c0    mov $r0.6 = 1431655765   ## 1431655765(I32)
;;								## 15
	c0    call $l0.0 = pow   ## bblock 3, line 37,  raddr(pow)(P32),  t218,  t219,  1070945621(I32),  1431655765(I32)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 16
	c0    stw 0xcc[$r0.1] = $r0.3  ## spill ## t220
	c0    NOP
	c0    mov $r0.5 = $r0.3   ## t220
	c0    mov $r0.6 = $r0.4   ## t221
;;								## 17
	c0    NOP
	c0    NOP
	c0    stw 0xd0[$r0.1] = $r0.4  ## spill ## t221
	c0    NOP
;;								## 18
	c0    ldw $r0.4 = 0x70[$r0.1]  ## restore ## t378
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 19
	c0    NOP
	c0    NOP
	c0    ldw $r0.3 = 0x6c[$r0.1]  ## restore ## t377
	c0    NOP
;;								## 21
	c0    call $l0.0 = _d_div   ## bblock 4, line 38,  raddr(_d_div)(P32),  t377,  t378,  t220,  t221
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 22
	c0    ldw $r0.5 = 0xcc[$r0.1]  ## restore ## t220
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 23
	c0    ldw $r0.6 = 0xd0[$r0.1]  ## restore ## t221
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 25
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_add   ## bblock 4, line 38,  raddr(_d_add)(P32),  t228,  t229,  t220,  t221
	c0    NOP
;;								## 26
	c0    mov $r0.5 = $r0.0   ## 0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    stw 0xd4[$r0.1] = $r0.3  ## spill ## t232
	c0    NOP
;;								## 27
	c0    NOP
	c0    NOP
	c0    NOP
	c0    stw 0xd8[$r0.1] = $r0.4  ## spill ## t233
;;								## 28
	c0    NOP
	c0    ldw $r0.4 = 0x90[$r0.1]  ## restore ## t376
	c0    NOP
	c0    NOP
;;								## 29
	c0    ldw $r0.3 = 0x8c[$r0.1]  ## restore ## t375
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 31
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_lt   ## bblock 4, line 39,  raddr(_d_lt)(P32),  t375,  t376,  0(I32),  0(I32)
	c0    NOP
;;								## 32
	c0    NOP
	c0    cmpne $b0.0 = $r0.3, $r0.0   ## bblock 4, line 39,  t386(I1),  t121,  0(I1)
	c0    mov $r0.2 = 1   ## 1(SI32)
	c0    NOP
;;								## 33
	c0    slct $r0.3 = $b0.0, $r0.2, -1   ## bblock 4, line 39,  t106,  t386(I1),  1(SI32),  -1(SI32)
	c0    NOP
	c0    call $l0.0 = _d_ilfloat   ## bblock 4, line 39,  raddr(_d_ilfloat)(P32),  t106
	c0    NOP
;;								## 34
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.5 = 0xd4[$r0.1]  ## restore ## t232
;;								## 35
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.6 = 0xd8[$r0.1]  ## restore ## t233
;;								## 37
	c0    NOP
	c0    call $l0.0 = _d_mul   ## bblock 4, line 39,  raddr(_d_mul)(P32),  t236,  t237,  t232,  t233
	c0    NOP
	c0    NOP
;;								## 38
	c0    NOP
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1074266112   ## 1074266112(I32)
	c0    stw 0xdc[$r0.1] = $r0.3  ## spill ## t240
;;								## 39
	c0    NOP
	c0    stw 0xe0[$r0.1] = $r0.4  ## spill ## t241
	c0    NOP
	c0    NOP
;;								## 40
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0x38[$r0.1]  ## restore ## t384
	c0    NOP
;;								## 41
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.3 = 0x34[$r0.1]  ## restore ## t383
;;								## 43
	c0    call $l0.0 = _d_div   ## bblock 4, line 40,  raddr(_d_div)(P32),  t383,  t384,  1074266112(I32),  0(I32)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 44
	c0    ldw $r0.3 = 0xdc[$r0.1]  ## restore ## t240
	c0    mov $r0.5 = $r0.3   ## t244
	c0    mov $r0.6 = $r0.4   ## t245
	c0    NOP
;;								## 45
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.4 = 0xe0[$r0.1]  ## restore ## t241
;;								## 47
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_sub   ## bblock 4, line 40,  raddr(_d_sub)(P32),  t240,  t241,  t244,  t245
	c0    NOP
;;								## 48
	c0    NOP
	c0    NOP
	c0    ldw $r0.2 = 0x2c[$r0.1]  ## restore ## t370
	c0    NOP
;;								## 49
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $l0.0 = 0x10[$r0.1]  ## restore ## t342
;;								## 51
	c0    NOP
	c0    NOP
	c0    stw 4[$r0.2] = $r0.4   ## bblock 4, line 40, t370, t247
	c0    NOP
;;								## 52
	c0    NOP
	c0    stw 0[$r0.2] = $r0.3   ## bblock 4, line 40, t370, t246
	c0    goto L1?3 ## goto
	c0    NOP
;;								## 53
	c0    NOP
	c0    add $r0.1 = $r0.1, (-0x20)
	c0    mov $r0.6 = $r0.0   ## bblock 0, line 48,  t43,  0(SI32)
	c0    mov $r0.7 = $r0.0   ## bblock 0, line 48,  t44,  0(SI32)
;;								## 0
	c0    mov $r0.2 = (~0x1f)   ## bblock 0, line 0,  t98,  (~0x1f)(I32)
	c0    mov $r0.13 = $r0.4   ## t40
	c0    mov $r0.14 = $l0.0   ## t26
	c0    stw 0x14[$r0.1] = $l0.0  ## spill ## t26
;;								## 1
	c0    stw 0x10[$r0.1] = $r0.0   ## bblock 0, line 47, t27, 0(SI32)
	c0    NOP
	c0    NOP
	c0    mov $r0.4 = (~0x3fffffff)   ## (~0x3fffffff)(I32)
;;								## 2
	c0    cmplt $b0.0 = $r0.2, $r0.0   ## bblock 1, line 53,  t115(I1),  t98,  0(SI32)
	c0    and $r0.5 = $r0.3, $r0.4   ## [spec] bblock 4, line 55,  t59,  t39,  (~0x3fffffff)(I32)
	c0    shl $r0.8 = $r0.7, 1   ## [spec] bblock 4, line 56,  t54,  t44,  1(SI32)
	c0    sh1add $r0.9 = $r0.7, 1   ## [spec] bblock 4, line 61,  t52,  t44,  1(SI32)
;;								## 0
	c0    shru $r0.5 = $r0.5, 30   ## [spec] bblock 4, line 55,  t5(I2),  t59,  30(SI32)
	c0    shl $r0.3 = $r0.3, 2   ## [spec] bblock 4, line 55,  t55,  t39,  2(SI32)
	c0    sh1add $r0.10 = $r0.8, 1   ## [spec] bblock 4, line 57,  t56,  t54,  1(SI32)
	c0    add $r0.2 = $r0.2, 4   ## [spec] bblock 4, line 0,  t98,  t98,  4(I32)
;;								## 1
	c0    sh2add $r0.6 = $r0.6, $r0.5   ## [spec] bblock 4, line 55,  t63,  t43,  t5(I2)
	c0    and $r0.5 = $r0.3, $r0.4   ## [spec] bblock 4, line 55-1,  t80,  t55,  (~0x3fffffff)(I32)
	c0    shl $r0.3 = $r0.3, 2   ## [spec] bblock 4, line 55-1,  t84,  t55,  2(SI32)
	c0    brf $b0.0, L1?3   ## bblock 1, line 53,  t115(I1)
;;								## 2
	c0    shru $r0.5 = $r0.5, 30   ## bblock 4, line 55-1,  t81(I2),  t80,  30(SI32)
	c0    and $r0.11 = $r0.3, $r0.4   ## bblock 4, line 55-2,  t64,  t84,  (~0x3fffffff)(I32)
	c0    cmpgeu $b0.0 = $r0.6, $r0.10   ## bblock 4, line 58,  t57(I1),  t63,  t56
	c0    sub $r0.10 = $r0.6, $r0.10   ## bblock 4, line 60,  t58,  t63,  t56
;;								## 3
	c0    shru $r0.11 = $r0.11, 30   ## bblock 4, line 55-2,  t67(I2),  t64,  30(SI32)
	c0    shl $r0.3 = $r0.3, 2   ## bblock 4, line 55-2,  t70,  t84,  2(SI32)
	c0    slct $r0.10 = $b0.0, $r0.10, $r0.6   ## bblock 4, line 60,  t60,  t57(I1),  t58,  t63
	c0    slct $r0.9 = $b0.0, $r0.9, $r0.8   ## bblock 4, line 61,  t61,  t57(I1),  t52,  t54
;;								## 4
	c0    and $r0.5 = $r0.3, $r0.4   ## bblock 4, line 55-3,  t4,  t70,  (~0x3fffffff)(I32)
	c0    sh2add $r0.10 = $r0.10, $r0.5   ## bblock 4, line 55-1,  t83,  t60,  t81(I2)
	c0    shl $r0.8 = $r0.9, 1   ## bblock 4, line 56-1,  t85,  t61,  1(SI32)
	c0    sh1add $r0.9 = $r0.9, 1   ## bblock 4, line 61-1,  t90,  t61,  1(SI32)
;;								## 5
	c0    shru $r0.5 = $r0.5, 30   ## bblock 4, line 55-3,  t53(I2),  t4,  30(SI32)
	c0    NOP
	c0    shl $r0.3 = $r0.3, 2   ## bblock 4, line 55-3,  t39,  t70,  2(SI32)
	c0    sh1add $r0.12 = $r0.8, 1   ## bblock 4, line 57-1,  t87,  t85,  1(SI32)
;;								## 6
	c0    NOP
	c0    NOP
	c0    cmpgeu $b0.0 = $r0.10, $r0.12   ## bblock 4, line 58-1,  t88(I1),  t83,  t87
	c0    sub $r0.12 = $r0.10, $r0.12   ## bblock 4, line 60-1,  t89,  t83,  t87
;;								## 7
	c0    NOP
	c0    slct $r0.12 = $b0.0, $r0.12, $r0.10   ## bblock 4, line 60-1,  t91,  t88(I1),  t89,  t83
	c0    NOP
	c0    slct $r0.9 = $b0.0, $r0.9, $r0.8   ## bblock 4, line 61-1,  t92,  t88(I1),  t90,  t85
;;								## 8
	c0    NOP
	c0    sh2add $r0.12 = $r0.12, $r0.11   ## bblock 4, line 55-2,  t69,  t91,  t67(I2)
	c0    shl $r0.8 = $r0.9, 1   ## bblock 4, line 56-2,  t71,  t92,  1(SI32)
	c0    sh1add $r0.9 = $r0.9, 1   ## bblock 4, line 61-2,  t76,  t92,  1(SI32)
;;								## 9
	c0    NOP
	c0    NOP
	c0    sh1add $r0.10 = $r0.8, 1   ## bblock 4, line 57-2,  t73,  t71,  1(SI32)
	c0    NOP
;;								## 10
	c0    NOP
	c0    NOP
	c0    cmpgeu $b0.0 = $r0.12, $r0.10   ## bblock 4, line 58-2,  t74(I1),  t69,  t73
	c0    sub $r0.10 = $r0.12, $r0.10   ## bblock 4, line 60-2,  t75,  t69,  t73
;;								## 11
	c0    NOP
	c0    NOP
	c0    slct $r0.10 = $b0.0, $r0.10, $r0.12   ## bblock 4, line 60-2,  t77,  t74(I1),  t75,  t69
	c0    slct $r0.9 = $b0.0, $r0.9, $r0.8   ## bblock 4, line 61-2,  t78,  t74(I1),  t76,  t71
;;								## 12
	c0    sh2add $r0.10 = $r0.10, $r0.5   ## bblock 4, line 55-3,  t50,  t77,  t53(I2)
	c0    shl $r0.8 = $r0.9, 1   ## bblock 4, line 56-3,  t51,  t78,  1(SI32)
	c0    NOP
	c0    sh1add $r0.9 = $r0.9, 1   ## bblock 4, line 61-3,  t48,  t78,  1(SI32)
;;								## 13
	c0    NOP
	c0    NOP
	c0    NOP
	c0    sh1add $r0.5 = $r0.8, 1   ## bblock 4, line 57-3,  t42,  t51,  1(SI32)
;;								## 14
	c0    NOP
	c0    cmpgeu $b0.0 = $r0.10, $r0.5   ## bblock 4, line 58-3,  t46(I1),  t50,  t42
	c0    sub $r0.5 = $r0.10, $r0.5   ## bblock 4, line 60-3,  t47,  t50,  t42
	c0    NOP
;;								## 15
	c0    slct $r0.6 = $b0.0, $r0.5, $r0.10   ## bblock 4, line 60-3,  t43,  t46(I1),  t47,  t50
	c0    slct $r0.7 = $b0.0, $r0.9, $r0.8   ## bblock 4, line 61-3,  t44,  t46(I1),  t48,  t51
	c0    NOP
	c0    goto L0?3 ## goto
;;								## 16
	c0    add $r0.4 = $r0.1, 0x10   ## bblock 14, line 64,  t25,  t27,  offset(a?1.2)=0x10(P32)
	c0    stw 0x14[$r0.1] = $r0.14  ## spill ## t26
	c0    mov $r0.5 = 4   ## 4(I32)
	c0    mov $r0.3 = $r0.13   ## t40
;;								## 0
	c0    stw 0x10[$r0.1] = $r0.7   ## bblock 14, line 0, t27, t44
	c0    NOP
	c0    call $l0.0 = memcpy   ## bblock 14, line 64,  raddr(memcpy)(P32),  t40,  t25,  4(I32)
	c0    NOP
;;								## 1
	c0    NOP
	c0    ldw $l0.0 = 0x14[$r0.1]  ## restore ## t26
	c0    NOP
	c0    NOP
;;								## 5
	c0    NOP
	c0    NOP
	c0    NOP
	c0    return $r0.1 = $r0.1, (0x20), $l0.0   ## bblock 3, line 65,  t27,  ?2.1?2auto_size(I32),  t26
;;								## 6
	c0    NOP
	c0    add $r0.1 = $r0.1, (-0x40)
	c0    NOP
	c0    NOP
;;								## 0
	c0    NOP
	c0    NOP
	c0    NOP
	c0    stw 0x10[$r0.1] = $l0.0  ## spill ## t15
;;								## 1
	c0    NOP
	c0    NOP
	c0    stw 0x14[$r0.1] = $r0.4  ## spill ## t29
	c0    NOP
;;								## 2
	c0    NOP
	c0    stw 0x18[$r0.1] = $r0.3  ## spill ## t28
	c0    mov $r0.4 = $r0.0   ## 0(I32)
	c0    NOP
;;								## 3
	c0    call $l0.0 = atan   ## bblock 0, line 15,  raddr(atan)(P32),  1072693248(I32),  0(I32)
	c0    mov $r0.3 = 1072693248   ## 1072693248(I32)
	c0    NOP
	c0    NOP
;;								## 4
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1074790400   ## 1074790400(I32)
	c0    NOP
	c0    stw 0x1c[$r0.1] = $r0.3  ## spill ## t5
;;								## 5
	c0    NOP
	c0    NOP
	c0    NOP
	c0    stw 0x20[$r0.1] = $r0.4  ## spill ## t6
;;								## 6
	c0    ldw $r0.4 = 0x14[$r0.1]  ## restore ## t29
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 7
	c0    ldw $r0.3 = 0x18[$r0.1]  ## restore ## t28
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 9
	c0    call $l0.0 = _d_div   ## bblock 1, line 15,  raddr(_d_div)(P32),  t28,  t29,  1074790400(I32),  0(I32)
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 10
	c0    NOP
	c0    ldw $r0.5 = 0x1c[$r0.1]  ## restore ## t5
	c0    NOP
	c0    NOP
;;								## 11
	c0    NOP
	c0    ldw $r0.6 = 0x20[$r0.1]  ## restore ## t6
	c0    NOP
	c0    NOP
;;								## 13
	c0    NOP
	c0    NOP
	c0    call $l0.0 = _d_div   ## bblock 1, line 15,  raddr(_d_div)(P32),  t9,  t10,  t5,  t6
	c0    NOP
;;								## 14
	c0    NOP
	c0    call $l0.0 = _d_mul   ## bblock 1, line 15,  raddr(_d_mul)(P32),  t11,  t12,  1080459264(I32),  0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1080459264   ## 1080459264(I32)
;;								## 15
	c0    NOP
	c0    NOP
	c0    ldw $l0.0 = 0x10[$r0.1]  ## restore ## t15
	c0    NOP
;;								## 19
	c0    NOP
	c0    return $r0.1 = $r0.1, (0x40), $l0.0   ## bblock 1, line 15,  t16,  ?2.1?2auto_size(I32),  t15
	c0    NOP
	c0    NOP
;;								## 20
	c0    NOP
	c0    NOP
	c0    NOP
	c0    add $r0.1 = $r0.1, (-0x40)
;;								## 0
	c0    stw 0x10[$r0.1] = $l0.0  ## spill ## t15
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 1
	c0    stw 0x14[$r0.1] = $r0.4  ## spill ## t6
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 2
	c0    stw 0x18[$r0.1] = $r0.3  ## spill ## t5
	c0    NOP
	c0    mov $r0.4 = $r0.0   ## 0(I32)
	c0    NOP
;;								## 3
	c0    NOP
	c0    NOP
	c0    call $l0.0 = atan   ## bblock 0, line 20,  raddr(atan)(P32),  1072693248(I32),  0(I32)
	c0    mov $r0.3 = 1072693248   ## 1072693248(I32)
;;								## 4
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    NOP
	c0    mov $r0.5 = 1080459264   ## 1080459264(I32)
	c0    stw 0x1c[$r0.1] = $r0.3  ## spill ## t7
;;								## 5
	c0    NOP
	c0    stw 0x20[$r0.1] = $r0.4  ## spill ## t8
	c0    NOP
	c0    NOP
;;								## 6
	c0    ldw $r0.4 = 0x14[$r0.1]  ## restore ## t6
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 7
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.3 = 0x18[$r0.1]  ## restore ## t5
;;								## 9
	c0    NOP
	c0    call $l0.0 = _d_div   ## bblock 1, line 20,  raddr(_d_div)(P32),  t5,  t6,  1080459264(I32),  0(I32)
	c0    NOP
	c0    NOP
;;								## 10
	c0    ldw $r0.5 = 0x1c[$r0.1]  ## restore ## t7
	c0    NOP
	c0    NOP
	c0    NOP
;;								## 11
	c0    NOP
	c0    NOP
	c0    NOP
	c0    ldw $r0.6 = 0x20[$r0.1]  ## restore ## t8
;;								## 13
	c0    NOP
	c0    call $l0.0 = _d_mul   ## bblock 1, line 20,  raddr(_d_mul)(P32),  t9,  t10,  t7,  t8
	c0    NOP
	c0    NOP
;;								## 14
	c0    NOP
	c0    call $l0.0 = _d_mul   ## bblock 1, line 20,  raddr(_d_mul)(P32),  t11,  t12,  1074790400(I32),  0(I32)
	c0    mov $r0.6 = $r0.0   ## 0(I32)
	c0    mov $r0.5 = 1074790400   ## 1074790400(I32)
;;								## 15
	c0    NOP
	c0    NOP
	c0    ldw $l0.0 = 0x10[$r0.1]  ## restore ## t15
	c0    NOP
;;								## 19
	c0    NOP
	c0    NOP
	c0    return $r0.1 = $r0.1, (0x40), $l0.0   ## bblock 1, line 20,  t16,  ?2.2?2auto_size(I32),  t15
	c0    NOP
;;								## 20
