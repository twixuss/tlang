format PE64 console
entry main
include 'win64a.inc'
section '.text' code readable executable
main:
push 0
call .0
pop rcx
and rsp, -16
sub rsp, 16
call [ExitProcess]
ret
.0: push rbp; bytecode.cpp:1138
.1: mov rbp, rsp; bytecode.cpp:1139
; lambda main;literal true
.2: push 1; bytecode.cpp:850
.3: pop r8; bytecode.cpp:877
.4: test r8, r8
jz .g; bytecode.cpp:878
; return;literal 1
.5: push 1; bytecode.cpp:864
.6: push rbp; bytecode.cpp:504
.7: add qword [rsp], 16; bytecode.cpp:505
.8: push rsp; bytecode.cpp:508
.9: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.a: pop r8; bytecode.cpp:384
.b: pop r9; bytecode.cpp:385
.c: mov r10, qword [r8]; bytecode.cpp:387
.d: mov qword [r9], r10; bytecode.cpp:388
.e: jmp .4w; bytecode.cpp:515
.f: jmp .g; bytecode.cpp:890
; literal true
.g: push 1; bytecode.cpp:850
.h: pop r8; bytecode.cpp:877
.i: test r8, r8
jz .u; bytecode.cpp:878
; return;literal 2
.j: push 2; bytecode.cpp:864
.k: push rbp; bytecode.cpp:504
.l: add qword [rsp], 16; bytecode.cpp:505
.m: push rsp; bytecode.cpp:508
.n: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.o: pop r8; bytecode.cpp:384
.p: pop r9; bytecode.cpp:385
.q: mov r10, qword [r8]; bytecode.cpp:387
.r: mov qword [r9], r10; bytecode.cpp:388
.s: jmp .4w; bytecode.cpp:515
.t: jmp .u; bytecode.cpp:890
; literal true
.u: push 1; bytecode.cpp:850
.v: pop r8; bytecode.cpp:877
.w: test r8, r8
jz .I; bytecode.cpp:878
; return;literal 3
.x: push 3; bytecode.cpp:864
.y: push rbp; bytecode.cpp:504
.z: add qword [rsp], 16; bytecode.cpp:505
.A: push rsp; bytecode.cpp:508
.B: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.C: pop r8; bytecode.cpp:384
.D: pop r9; bytecode.cpp:385
.E: mov r10, qword [r8]; bytecode.cpp:387
.F: mov qword [r9], r10; bytecode.cpp:388
.G: jmp .4w; bytecode.cpp:515
.H: jmp .I; bytecode.cpp:890
; literal true
.I: push 1; bytecode.cpp:850
.J: pop r8; bytecode.cpp:877
.K: test r8, r8
jz .W; bytecode.cpp:878
; return;literal 3
.L: push 3; bytecode.cpp:864
.M: push rbp; bytecode.cpp:504
.N: add qword [rsp], 16; bytecode.cpp:505
.O: push rsp; bytecode.cpp:508
.P: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.Q: pop r8; bytecode.cpp:384
.R: pop r9; bytecode.cpp:385
.S: mov r10, qword [r8]; bytecode.cpp:387
.T: mov qword [r9], r10; bytecode.cpp:388
.U: jmp .4w; bytecode.cpp:515
.V: jmp .W; bytecode.cpp:890
; literal true
.W: push 1; bytecode.cpp:850
.X: pop r8; bytecode.cpp:877
.Y: test r8, r8
jz .1a; bytecode.cpp:878
; return;literal 4
.Z: push 4; bytecode.cpp:864
.10: push rbp; bytecode.cpp:504
.11: add qword [rsp], 16; bytecode.cpp:505
.12: push rsp; bytecode.cpp:508
.13: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.14: pop r8; bytecode.cpp:384
.15: pop r9; bytecode.cpp:385
.16: mov r10, qword [r8]; bytecode.cpp:387
.17: mov qword [r9], r10; bytecode.cpp:388
.18: jmp .4w; bytecode.cpp:515
.19: jmp .1k; bytecode.cpp:890
; return;literal 5
.1a: push 5; bytecode.cpp:864
.1b: push rbp; bytecode.cpp:504
.1c: add qword [rsp], 16; bytecode.cpp:505
.1d: push rsp; bytecode.cpp:508
.1e: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.1f: pop r8; bytecode.cpp:384
.1g: pop r9; bytecode.cpp:385
.1h: mov r10, qword [r8]; bytecode.cpp:387
.1i: mov qword [r9], r10; bytecode.cpp:388
.1j: jmp .4w; bytecode.cpp:515
; literal true
.1k: push 1; bytecode.cpp:850
.1l: pop r8; bytecode.cpp:877
.1m: test r8, r8
jz .1y; bytecode.cpp:878
; return;literal 4
.1n: push 4; bytecode.cpp:864
.1o: push rbp; bytecode.cpp:504
.1p: add qword [rsp], 16; bytecode.cpp:505
.1q: push rsp; bytecode.cpp:508
.1r: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.1s: pop r8; bytecode.cpp:384
.1t: pop r9; bytecode.cpp:385
.1u: mov r10, qword [r8]; bytecode.cpp:387
.1v: mov qword [r9], r10; bytecode.cpp:388
.1w: jmp .4w; bytecode.cpp:515
.1x: jmp .1I; bytecode.cpp:890
; return;literal 5
.1y: push 5; bytecode.cpp:864
.1z: push rbp; bytecode.cpp:504
.1A: add qword [rsp], 16; bytecode.cpp:505
.1B: push rsp; bytecode.cpp:508
.1C: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.1D: pop r8; bytecode.cpp:384
.1E: pop r9; bytecode.cpp:385
.1F: mov r10, qword [r8]; bytecode.cpp:387
.1G: mov qword [r9], r10; bytecode.cpp:388
.1H: jmp .4w; bytecode.cpp:515
; literal true
.1I: push 1; bytecode.cpp:850
.1J: pop r8; bytecode.cpp:877
.1K: test r8, r8
jz .1W; bytecode.cpp:878
; return;literal 4
.1L: push 4; bytecode.cpp:864
.1M: push rbp; bytecode.cpp:504
.1N: add qword [rsp], 16; bytecode.cpp:505
.1O: push rsp; bytecode.cpp:508
.1P: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.1Q: pop r8; bytecode.cpp:384
.1R: pop r9; bytecode.cpp:385
.1S: mov r10, qword [r8]; bytecode.cpp:387
.1T: mov qword [r9], r10; bytecode.cpp:388
.1U: jmp .4w; bytecode.cpp:515
.1V: jmp .26; bytecode.cpp:890
; return;literal 5
.1W: push 5; bytecode.cpp:864
.1X: push rbp; bytecode.cpp:504
.1Y: add qword [rsp], 16; bytecode.cpp:505
.1Z: push rsp; bytecode.cpp:508
.20: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.21: pop r8; bytecode.cpp:384
.22: pop r9; bytecode.cpp:385
.23: mov r10, qword [r8]; bytecode.cpp:387
.24: mov qword [r9], r10; bytecode.cpp:388
.25: jmp .4w; bytecode.cpp:515
; literal true
.26: push 1; bytecode.cpp:850
.27: pop r8; bytecode.cpp:877
.28: test r8, r8
jz .2k; bytecode.cpp:878
; return;literal 4
.29: push 4; bytecode.cpp:864
.2a: push rbp; bytecode.cpp:504
.2b: add qword [rsp], 16; bytecode.cpp:505
.2c: push rsp; bytecode.cpp:508
.2d: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.2e: pop r8; bytecode.cpp:384
.2f: pop r9; bytecode.cpp:385
.2g: mov r10, qword [r8]; bytecode.cpp:387
.2h: mov qword [r9], r10; bytecode.cpp:388
.2i: jmp .4w; bytecode.cpp:515
.2j: jmp .2u; bytecode.cpp:890
; return;literal 5
.2k: push 5; bytecode.cpp:864
.2l: push rbp; bytecode.cpp:504
.2m: add qword [rsp], 16; bytecode.cpp:505
.2n: push rsp; bytecode.cpp:508
.2o: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.2p: pop r8; bytecode.cpp:384
.2q: pop r9; bytecode.cpp:385
.2r: mov r10, qword [r8]; bytecode.cpp:387
.2s: mov qword [r9], r10; bytecode.cpp:388
.2t: jmp .4w; bytecode.cpp:515
; literal true
.2u: push 1; bytecode.cpp:850
.2v: pop r8; bytecode.cpp:877
.2w: test r8, r8
jz .2I; bytecode.cpp:878
; return;literal 4
.2x: push 4; bytecode.cpp:864
.2y: push rbp; bytecode.cpp:504
.2z: add qword [rsp], 16; bytecode.cpp:505
.2A: push rsp; bytecode.cpp:508
.2B: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.2C: pop r8; bytecode.cpp:384
.2D: pop r9; bytecode.cpp:385
.2E: mov r10, qword [r8]; bytecode.cpp:387
.2F: mov qword [r9], r10; bytecode.cpp:388
.2G: jmp .4w; bytecode.cpp:515
.2H: jmp .2S; bytecode.cpp:890
; return;literal 5
.2I: push 5; bytecode.cpp:864
.2J: push rbp; bytecode.cpp:504
.2K: add qword [rsp], 16; bytecode.cpp:505
.2L: push rsp; bytecode.cpp:508
.2M: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.2N: pop r8; bytecode.cpp:384
.2O: pop r9; bytecode.cpp:385
.2P: mov r10, qword [r8]; bytecode.cpp:387
.2Q: mov qword [r9], r10; bytecode.cpp:388
.2R: jmp .4w; bytecode.cpp:515
; literal true
.2S: push 1; bytecode.cpp:850
.2T: pop r8; bytecode.cpp:877
.2U: test r8, r8
jz .36; bytecode.cpp:878
; return;literal 4
.2V: push 4; bytecode.cpp:864
.2W: push rbp; bytecode.cpp:504
.2X: add qword [rsp], 16; bytecode.cpp:505
.2Y: push rsp; bytecode.cpp:508
.2Z: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.30: pop r8; bytecode.cpp:384
.31: pop r9; bytecode.cpp:385
.32: mov r10, qword [r8]; bytecode.cpp:387
.33: mov qword [r9], r10; bytecode.cpp:388
.34: jmp .4w; bytecode.cpp:515
.35: jmp .3g; bytecode.cpp:890
; return;literal 5
.36: push 5; bytecode.cpp:864
.37: push rbp; bytecode.cpp:504
.38: add qword [rsp], 16; bytecode.cpp:505
.39: push rsp; bytecode.cpp:508
.3a: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.3b: pop r8; bytecode.cpp:384
.3c: pop r9; bytecode.cpp:385
.3d: mov r10, qword [r8]; bytecode.cpp:387
.3e: mov qword [r9], r10; bytecode.cpp:388
.3f: jmp .4w; bytecode.cpp:515
; literal true
.3g: push 1; bytecode.cpp:850
.3h: pop r8; bytecode.cpp:877
.3i: test r8, r8
jz .3u; bytecode.cpp:878
; return;literal 4
.3j: push 4; bytecode.cpp:864
.3k: push rbp; bytecode.cpp:504
.3l: add qword [rsp], 16; bytecode.cpp:505
.3m: push rsp; bytecode.cpp:508
.3n: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.3o: pop r8; bytecode.cpp:384
.3p: pop r9; bytecode.cpp:385
.3q: mov r10, qword [r8]; bytecode.cpp:387
.3r: mov qword [r9], r10; bytecode.cpp:388
.3s: jmp .4w; bytecode.cpp:515
.3t: jmp .3E; bytecode.cpp:890
; return;literal 5
.3u: push 5; bytecode.cpp:864
.3v: push rbp; bytecode.cpp:504
.3w: add qword [rsp], 16; bytecode.cpp:505
.3x: push rsp; bytecode.cpp:508
.3y: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.3z: pop r8; bytecode.cpp:384
.3A: pop r9; bytecode.cpp:385
.3B: mov r10, qword [r8]; bytecode.cpp:387
.3C: mov qword [r9], r10; bytecode.cpp:388
.3D: jmp .4w; bytecode.cpp:515
; literal true
.3E: push 1; bytecode.cpp:850
.3F: pop r8; bytecode.cpp:877
.3G: test r8, r8
jz .3S; bytecode.cpp:878
; return;literal 4
.3H: push 4; bytecode.cpp:864
.3I: push rbp; bytecode.cpp:504
.3J: add qword [rsp], 16; bytecode.cpp:505
.3K: push rsp; bytecode.cpp:508
.3L: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.3M: pop r8; bytecode.cpp:384
.3N: pop r9; bytecode.cpp:385
.3O: mov r10, qword [r8]; bytecode.cpp:387
.3P: mov qword [r9], r10; bytecode.cpp:388
.3Q: jmp .4w; bytecode.cpp:515
.3R: jmp .42; bytecode.cpp:890
; return;literal 5
.3S: push 5; bytecode.cpp:864
.3T: push rbp; bytecode.cpp:504
.3U: add qword [rsp], 16; bytecode.cpp:505
.3V: push rsp; bytecode.cpp:508
.3W: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.3X: pop r8; bytecode.cpp:384
.3Y: pop r9; bytecode.cpp:385
.3Z: mov r10, qword [r8]; bytecode.cpp:387
.40: mov qword [r9], r10; bytecode.cpp:388
.41: jmp .4w; bytecode.cpp:515
; return;literal true
.42: push 1; bytecode.cpp:850
.43: pop r8; bytecode.cpp:1246
.44: test r8, r8
jz .47; bytecode.cpp:1247
; literal 1
.45: push 1; bytecode.cpp:864
.46: jmp .48; bytecode.cpp:1251
; literal 2
.47: push 2; bytecode.cpp:864
.48: push rbp; bytecode.cpp:504
.49: add qword [rsp], 16; bytecode.cpp:505
.4a: push rsp; bytecode.cpp:508
.4b: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.4c: pop r8; bytecode.cpp:384
.4d: pop r9; bytecode.cpp:385
.4e: mov r10, qword [r8]; bytecode.cpp:387
.4f: mov qword [r9], r10; bytecode.cpp:388
.4g: jmp .4w; bytecode.cpp:515
; return;literal true
.4h: push 1; bytecode.cpp:850
.4i: pop r8; bytecode.cpp:1246
.4j: test r8, r8
jz .4m; bytecode.cpp:1247
; literal 1
.4k: push 1; bytecode.cpp:864
.4l: jmp .4n; bytecode.cpp:1251
; literal 2
.4m: push 2; bytecode.cpp:864
.4n: push rbp; bytecode.cpp:504
.4o: add qword [rsp], 16; bytecode.cpp:505
.4p: push rsp; bytecode.cpp:508
.4q: add qword [rsp], 8; bytecode.cpp:509
; copy 8 bytes from expression into parameter, reverse=false
.4r: pop r8; bytecode.cpp:384
.4s: pop r9; bytecode.cpp:385
.4t: mov r10, qword [r8]; bytecode.cpp:387
.4u: mov qword [r9], r10; bytecode.cpp:388
.4v: jmp .4w; bytecode.cpp:515
.4w: mov rsp, rbp; bytecode.cpp:1202
.4x: pop rbp; bytecode.cpp:1203
.4y: ret; bytecode.cpp:1226
section '.idata' import data readable writeable
library kernel32,'kernel32.dll'
import kernel32,ExitProcess,'ExitProcess'
