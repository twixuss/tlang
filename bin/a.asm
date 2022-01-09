format PE64 GUI 4.0
entry main
include 'win64a.inc'
section '.text' code readable executable
main:
push 0
call .cp
pop rcx
and rsp, -16
sub rsp, 16
call [ExitProcess]
ret
.0: push rbp
.1: mov rbp, rsp
.2: mov r8, rsp
.3: and rsp, -16
.4: push r8
.5: push 0
.6: push 0
.7: sub rsp, 8
.8: sub rsp, 16
.9: push rsp
.a: push rbp
.b: add qword [rsp], 16
.c: mov rcx, 16
pop rsi
pop rdi
cld
rep movsb
.d: push rsp
.e: add qword [rsp], 16
.f: push rsp
.g: add qword [rsp], 16
.h: pop r8
.i: pop r9
.j: mov r10, qword [r8]
.k: mov qword [r9], r10
.l: add rsp, 16
.m: sub rsp, 8
.n: sub rsp, 16
.o: push rsp
.p: push rbp
.q: add qword [rsp], 16
.r: mov rcx, 16
pop rsi
pop rdi
cld
rep movsb
.s: push rsp
.t: add qword [rsp], 16
.u: push rsp
.v: add qword [rsp], 8
.w: pop r8
.x: pop r9
.y: mov r10, qword [r8]
.z: mov qword [r9], r10
.A: add rsp, 16
.B: mov r8, rsp
.C: and rsp, -16
.D: push r8
.E: sub rsp, 8
.F: push -11
.G: pop r8
.H: sub rsp, 32
.I: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call [GetStdHandle]
.J: add rsp, 40
.K: pop rsp
.L: push rax
.M: pop r8
.N: pop r9
.O: pop r10
.P: pop r11
.Q: sub rsp, 32
.R: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call [WriteConsoleA]
.S: add rsp, 40
.T: pop rsp
.U: push rax
.V: add rsp, 8
.W: mov rsp, rbp
.X: pop rbp
.Y: ret
.Z: push rbp
.10: mov rbp, rsp
.11: mov r8, rsp
.12: and rsp, -16
.13: push r8
.14: push 0
.15: push 0
.16: push 1
.17: push rbp
.18: add qword [rsp], 16
.19: mov r8, rsp
.1a: and rsp, -16
.1b: push r8
.1c: sub rsp, 8
.1d: push -11
.1e: pop r8
.1f: sub rsp, 32
.1g: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call [GetStdHandle]
.1h: add rsp, 40
.1i: pop rsp
.1j: push rax
.1k: pop r8
.1l: pop r9
.1m: pop r10
.1n: pop r11
.1o: sub rsp, 32
.1p: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call [WriteConsoleA]
.1q: add rsp, 40
.1r: pop rsp
.1s: push rax
.1t: add rsp, 8
.1u: mov rsp, rbp
.1v: pop rbp
.1w: ret
.1x: push rbp
.1y: mov rbp, rsp
.1z: sub rsp, 8
.1A: push rsp
.1B: push rbp
.1C: add qword [rsp], 16
.1D: pop r8
.1E: pop r9
.1F: mov r10, qword [r8]
.1G: mov qword [r9], r10
.1H: sub rsp, 8
.1I: push rsp
.1J: push rbp
.1K: add qword [rsp], -8
.1L: pop r8
.1M: pop r9
.1N: mov r10, qword [r8]
.1O: mov qword [r9], r10
.1P: push 0
.1Q: pop r9
.1R: pop r8
.1S: xor r10, r10
cmp r8, r9
sete r10b
.1T: push r10
.1U: pop r8
.1V: test r8, r8
jz .21
.1W: push 48
.1X: call .Z
.1Y: add rsp, 8
.1Z: jmp .5I
.20: jmp .21
.21: sub rsp, 8
.22: push rsp
.23: push rbp
.24: add qword [rsp], -8
.25: pop r8
.26: pop r9
.27: mov r10, qword [r8]
.28: mov qword [r9], r10
.29: push 0
.2a: pop r9
.2b: pop r8
.2c: xor r10, r10
cmp r8, r9
setl r10b
.2d: push r10
.2e: pop r8
.2f: test r8, r8
jz .2F
.2g: push 45
.2h: call .Z
.2i: add rsp, 8
.2j: sub rsp, 8
.2k: push rsp
.2l: push rbp
.2m: add qword [rsp], -8
.2n: pop r8
.2o: pop r9
.2p: mov r10, qword [r8]
.2q: mov qword [r9], r10
.2r: pop r9
.2s: xor r8, r8
.2t: sub r8, r9
.2u: push r8
.2v: push rbp
.2w: add qword [rsp], -8
.2x: push rsp
.2y: add qword [rsp], 8
.2z: pop r8
.2A: pop r9
.2B: mov r10, qword [r8]
.2C: mov qword [r9], r10
.2D: add rsp, 8
.2E: jmp .2F
.2F: sub rsp, 8
.2G: push rsp
.2H: push rbp
.2I: add qword [rsp], -8
.2J: pop r8
.2K: pop r9
.2L: mov r10, qword [r8]
.2M: mov qword [r9], r10
.2N: push 0
.2O: push 0
.2P: push 0
.2Q: push 0
.2R: push 0
.2S: push 0
.2T: push 0
.2U: push 0
.2V: push 0
.2W: push 0
.2X: push 0
.2Y: push 0
.2Z: push 0
.30: push 0
.31: push 0
.32: push 0
.33: push 0
.34: push 0
.35: push 0
.36: push 0
.37: push 0
.38: push 0
.39: push 0
.3a: push 0
.3b: push 0
.3c: push 0
.3d: push 0
.3e: push 0
.3f: push 0
.3g: push 0
.3h: push 0
.3i: push 0
.3j: push 0
.3k: push 0
.3l: push 0
.3m: push 0
.3n: push 0
.3o: push 0
.3p: push 0
.3q: push 0
.3r: push 0
.3s: push 0
.3t: push 0
.3u: push 0
.3v: push 0
.3w: push 0
.3x: push 0
.3y: push 0
.3z: push 0
.3A: push 0
.3B: push 0
.3C: push 0
.3D: push 0
.3E: push 0
.3F: push 0
.3G: push 0
.3H: push 0
.3I: push 0
.3J: push 0
.3K: push 0
.3L: push 0
.3M: push 0
.3N: push 0
.3O: push 0
.3P: push 64
.3Q: sub rsp, 8
.3R: push rsp
.3S: push rbp
.3T: add qword [rsp], -16
.3U: pop r8
.3V: pop r9
.3W: mov r10, qword [r8]
.3X: mov qword [r9], r10
.3Y: push 0
.3Z: pop r9
.40: pop r8
.41: xor r10, r10
cmp r8, r9
setne r10b
.42: push r10
.43: pop r8
.44: test r8, r8
jz .4Z
.45: sub rsp, 8
.46: push rsp
.47: push rbp
.48: add qword [rsp], -16
.49: pop r8
.4a: pop r9
.4b: mov r10, qword [r8]
.4c: mov qword [r9], r10
.4d: push 10
.4e: pop r8
.4f: mov rdx, 0
mov rax, qword[rsp]
div r8
mov qword[rsp], rdx
.4g: push 1
.4h: push rbp
.4i: add qword [rsp], -536
.4j: pop r8
.4k: pop r9
.4l: sub qword [r8], r9
.4m: sub rsp, 8
.4n: push rsp
.4o: push rbp
.4p: add qword [rsp], -544
.4q: pop r8
.4r: pop r9
.4s: mov r10, qword [r8]
.4t: mov qword [r9], r10
.4u: push 48
.4v: pop r8
.4w: add qword [rsp], r8
.4x: push rbp
.4y: add qword [rsp], -528
.4z: sub rsp, 8
.4A: push rsp
.4B: push rbp
.4C: add qword [rsp], -536
.4D: pop r8
.4E: pop r9
.4F: mov r10, qword [r8]
.4G: mov qword [r9], r10
.4H: pop r8
.4I: shl r8, 3
.4J: add qword [rsp], r8
.4K: push rsp
.4L: add qword [rsp], 8
.4M: pop r8
.4N: pop r9
.4O: mov r10, qword [r8]
.4P: mov qword [r9], r10
.4Q: add rsp, 8
.4R: push 10
.4S: push rbp
.4T: add qword [rsp], -16
.4U: pop r8
.4V: pop r9
.4W: mov rdx, 0
mov rax, qword[r8]
div r9
mov qword[r8], rax
.4X: add rsp, 8
.4Y: jmp .3Q
.4Z: sub rsp, 8
.50: push rsp
.51: push rbp
.52: add qword [rsp], -536
.53: pop r8
.54: pop r9
.55: mov r10, qword [r8]
.56: mov qword [r9], r10
.57: push 64
.58: pop r9
.59: pop r8
.5a: xor r10, r10
cmp r8, r9
setne r10b
.5b: push r10
.5c: pop r8
.5d: test r8, r8
jz .5I
.5e: sub rsp, 8
.5f: push rsp
.5g: push rbp
.5h: add qword [rsp], -536
.5i: pop r8
.5j: pop r9
.5k: mov r10, qword [r8]
.5l: mov qword [r9], r10
.5m: push rbp
.5n: add qword [rsp], -528
.5o: pop r8
.5p: pop r9
.5q: shl r9, 3
.5r: add r8, r9
.5s: sub rsp, 8
.5t: push rsp
.5u: push r8
.5v: pop r8
.5w: pop r9
.5x: mov r10, qword [r8]
.5y: mov qword [r9], r10
.5z: call .Z
.5A: add rsp, 8
.5B: push 1
.5C: push rbp
.5D: add qword [rsp], -536
.5E: pop r8
.5F: pop r9
.5G: add qword [r8], r9
.5H: jmp .4Z
.5I: mov rsp, rbp
.5J: pop rbp
.5K: ret
.5L: push rbp
.5M: mov rbp, rsp
.5N: sub rsp, 8
.5O: push rsp
.5P: push rbp
.5Q: add qword [rsp], 16
.5R: pop r8
.5S: pop r9
.5T: mov r10, qword [r8]
.5U: mov qword [r9], r10
.5V: sub rsp, 8
.5W: push rsp
.5X: push rbp
.5Y: add qword [rsp], -8
.5Z: pop r8
.60: pop r9
.61: mov r10, qword [r8]
.62: mov qword [r9], r10
.63: push 0
.64: pop r9
.65: pop r8
.66: xor r10, r10
cmp r8, r9
sete r10b
.67: push r10
.68: pop r8
.69: test r8, r8
jz .6f
.6a: push 48
.6b: call .Z
.6c: add rsp, 8
.6d: jmp .a6
.6e: jmp .6f
.6f: sub rsp, 8
.6g: push rsp
.6h: push rbp
.6i: add qword [rsp], -8
.6j: pop r8
.6k: pop r9
.6l: mov r10, qword [r8]
.6m: mov qword [r9], r10
.6n: push 0
.6o: push 0
.6p: push 0
.6q: push 0
.6r: push 0
.6s: push 0
.6t: push 0
.6u: push 0
.6v: push 0
.6w: push 0
.6x: push 0
.6y: push 0
.6z: push 0
.6A: push 0
.6B: push 0
.6C: push 0
.6D: push 0
.6E: push 0
.6F: push 0
.6G: push 0
.6H: push 0
.6I: push 0
.6J: push 0
.6K: push 0
.6L: push 0
.6M: push 0
.6N: push 0
.6O: push 0
.6P: push 0
.6Q: push 0
.6R: push 0
.6S: push 0
.6T: push 0
.6U: push 0
.6V: push 0
.6W: push 0
.6X: push 0
.6Y: push 0
.6Z: push 0
.70: push 0
.71: push 0
.72: push 0
.73: push 0
.74: push 0
.75: push 0
.76: push 0
.77: push 0
.78: push 0
.79: push 0
.7a: push 0
.7b: push 0
.7c: push 0
.7d: push 0
.7e: push 0
.7f: push 0
.7g: push 0
.7h: push 0
.7i: push 0
.7j: push 0
.7k: push 0
.7l: push 0
.7m: push 0
.7n: push 0
.7o: push 0
.7p: push 64
.7q: sub rsp, 8
.7r: push rsp
.7s: push rbp
.7t: add qword [rsp], -16
.7u: pop r8
.7v: pop r9
.7w: mov r10, qword [r8]
.7x: mov qword [r9], r10
.7y: push 0
.7z: pop r9
.7A: pop r8
.7B: xor r10, r10
cmp r8, r9
setne r10b
.7C: push r10
.7D: pop r8
.7E: test r8, r8
jz .9n
.7F: sub rsp, 8
.7G: push rsp
.7H: push rbp
.7I: add qword [rsp], -16
.7J: pop r8
.7K: pop r9
.7L: mov r10, qword [r8]
.7M: mov qword [r9], r10
.7N: push 15
.7O: pop r8
.7P: and qword [rsp], r8
.7Q: push 1
.7R: push rbp
.7S: add qword [rsp], -536
.7T: pop r8
.7U: pop r9
.7V: sub qword [r8], r9
.7W: sub rsp, 8
.7X: push rsp
.7Y: push rbp
.7Z: add qword [rsp], -544
.80: pop r8
.81: pop r9
.82: mov r10, qword [r8]
.83: mov qword [r9], r10
.84: push 10
.85: pop r9
.86: pop r8
.87: xor r10, r10
cmp r8, r9
setl r10b
.88: push r10
.89: pop r8
.8a: test r8, r8
jz .8H
.8b: sub rsp, 8
.8c: push rsp
.8d: push rbp
.8e: add qword [rsp], -544
.8f: pop r8
.8g: pop r9
.8h: mov r10, qword [r8]
.8i: mov qword [r9], r10
.8j: push 48
.8k: pop r8
.8l: add qword [rsp], r8
.8m: push rbp
.8n: add qword [rsp], -528
.8o: sub rsp, 8
.8p: push rsp
.8q: push rbp
.8r: add qword [rsp], -536
.8s: pop r8
.8t: pop r9
.8u: mov r10, qword [r8]
.8v: mov qword [r9], r10
.8w: pop r8
.8x: shl r8, 3
.8y: add qword [rsp], r8
.8z: push rsp
.8A: add qword [rsp], 8
.8B: pop r8
.8C: pop r9
.8D: mov r10, qword [r8]
.8E: mov qword [r9], r10
.8F: add rsp, 8
.8G: jmp .9f
.8H: sub rsp, 8
.8I: push rsp
.8J: push rbp
.8K: add qword [rsp], -544
.8L: pop r8
.8M: pop r9
.8N: mov r10, qword [r8]
.8O: mov qword [r9], r10
.8P: push 10
.8Q: pop r8
.8R: sub qword [rsp], r8
.8S: push 97
.8T: pop r8
.8U: add qword [rsp], r8
.8V: push rbp
.8W: add qword [rsp], -528
.8X: sub rsp, 8
.8Y: push rsp
.8Z: push rbp
.90: add qword [rsp], -536
.91: pop r8
.92: pop r9
.93: mov r10, qword [r8]
.94: mov qword [r9], r10
.95: pop r8
.96: shl r8, 3
.97: add qword [rsp], r8
.98: push rsp
.99: add qword [rsp], 8
.9a: pop r8
.9b: pop r9
.9c: mov r10, qword [r8]
.9d: mov qword [r9], r10
.9e: add rsp, 8
.9f: push 4
.9g: push rbp
.9h: add qword [rsp], -16
.9i: pop r8
.9j: pop r9
.9k: mov cl, r9b
shr qword[r8], cl
.9l: add rsp, 8
.9m: jmp .7q
.9n: sub rsp, 8
.9o: push rsp
.9p: push rbp
.9q: add qword [rsp], -536
.9r: pop r8
.9s: pop r9
.9t: mov r10, qword [r8]
.9u: mov qword [r9], r10
.9v: push 64
.9w: pop r9
.9x: pop r8
.9y: xor r10, r10
cmp r8, r9
setne r10b
.9z: push r10
.9A: pop r8
.9B: test r8, r8
jz .a6
.9C: sub rsp, 8
.9D: push rsp
.9E: push rbp
.9F: add qword [rsp], -536
.9G: pop r8
.9H: pop r9
.9I: mov r10, qword [r8]
.9J: mov qword [r9], r10
.9K: push rbp
.9L: add qword [rsp], -528
.9M: pop r8
.9N: pop r9
.9O: shl r9, 3
.9P: add r8, r9
.9Q: sub rsp, 8
.9R: push rsp
.9S: push r8
.9T: pop r8
.9U: pop r9
.9V: mov r10, qword [r8]
.9W: mov qword [r9], r10
.9X: call .Z
.9Y: add rsp, 8
.9Z: push 1
.a0: push rbp
.a1: add qword [rsp], -536
.a2: pop r8
.a3: pop r9
.a4: add qword [r8], r9
.a5: jmp .9n
.a6: mov rsp, rbp
.a7: pop rbp
.a8: ret
.a9: push rbp
.aa: mov rbp, rsp
.ab: sub rsp, 8
.ac: push rsp
.ad: push rbp
.ae: add qword [rsp], 16
.af: pop r8
.ag: pop r9
.ah: mov r10b, byte [r8]
.ai: mov byte [r9], r10b
.aj: push 8
.ak: pop r8
.al: mov cl, r8b
shl qword[rsp], cl
.am: sub rsp, 8
.an: push rsp
.ao: push rbp
.ap: add qword [rsp], 24
.aq: pop r8
.ar: pop r9
.as: mov r10b, byte [r8]
.at: mov byte [r9], r10b
.au: pop r8
.av: or qword [rsp], r8
.aw: and qword [rsp], 255
.ax: push rbp
.ay: add qword [rsp], 32
.az: push rsp
.aA: add qword [rsp], 8
.aB: pop r8
.aC: pop r9
.aD: mov r10w, word [r8]
.aE: mov word [r9], r10w
.aF: jmp .aG
.aG: mov rsp, rbp
.aH: pop rbp
.aI: ret
.aJ: push rbp
.aK: mov rbp, rsp
.aL: sub rsp, 8
push r9
push r8
push rdx
push rcx
push 0x12345678
push rbp
mov rbp, rsp
.aM: sub rsp, 8
.aN: push rsp
.aO: push rbp
.aP: add qword [rsp], 24
.aQ: pop r8
.aR: pop r9
.aS: mov r10d, dword [r8]
.aT: mov dword [r9], r10d
.aU: sub rsp, 8
.aV: push rsp
.aW: mov rax, constants + 584
push rax
.aX: pop r8
.aY: pop r9
.aZ: mov r10d, dword [r8]
.b0: mov dword [r9], r10d
.b1: pop r9
.b2: pop r8
.b3: xor r10, r10
cmp r8d, r9d
sete r10b
.b4: push r10
.b5: pop r8
.b6: test r8, r8
jz .bt
.b7: mov r8, rsp
.b8: and rsp, -16
.b9: push r8
.ba: sub rsp, 8
.bb: push 0
.bc: pop r8
.bd: sub rsp, 32
.be: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call [PostQuitMessage]
.bf: add rsp, 40
.bg: pop rsp
.bh: push rax
.bi: push 0
.bj: push rbp
.bk: add qword [rsp], 48
.bl: push rsp
.bm: add qword [rsp], 8
.bn: pop r8
.bo: pop r9
.bp: mov r10, qword [r8]
.bq: mov qword [r9], r10
.br: jmp .cl
.bs: jmp .bt
.bt: mov r8, rsp
.bu: and rsp, -16
.bv: push r8
.bw: sub rsp, 8
.bx: sub rsp, 8
.by: push rsp
.bz: push rbp
.bA: add qword [rsp], 40
.bB: pop r8
.bC: pop r9
.bD: mov r10, qword [r8]
.bE: mov qword [r9], r10
.bF: sub rsp, 8
.bG: push rsp
.bH: push rbp
.bI: add qword [rsp], 32
.bJ: pop r8
.bK: pop r9
.bL: mov r10, qword [r8]
.bM: mov qword [r9], r10
.bN: sub rsp, 8
.bO: push rsp
.bP: push rbp
.bQ: add qword [rsp], 24
.bR: pop r8
.bS: pop r9
.bT: mov r10d, dword [r8]
.bU: mov dword [r9], r10d
.bV: sub rsp, 8
.bW: push rsp
.bX: push rbp
.bY: add qword [rsp], 16
.bZ: pop r8
.c0: pop r9
.c1: mov r10, qword [r8]
.c2: mov qword [r9], r10
.c3: pop r8
.c4: pop r9
.c5: pop r10
.c6: pop r11
.c7: sub rsp, 32
.c8: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call [DefWindowProcA]
.c9: add rsp, 40
.ca: pop rsp
.cb: push rax
.cc: push rbp
.cd: add qword [rsp], 48
.ce: push rsp
.cf: add qword [rsp], 8
.cg: pop r8
.ch: pop r9
.ci: mov r10, qword [r8]
.cj: mov qword [r9], r10
.ck: jmp .cl
.cl: mov rsp, rbp
.cm: pop rbp
.cn: add rsp, 40
pop rax
mov rsp, rbp
pop rbp
.co: ret
.cp: push rbp
.cq: mov rbp, rsp
.cr: sub rsp, 8
.cs: push 13
.ct: mov rax, constants + 736
push rax
.cu: push rsp
.cv: add qword [rsp], 16
.cw: push rsp
.cx: add qword [rsp], 8
.cy: pop r8
.cz: pop r9
.cA: mov r10, qword [r8]
.cB: mov qword [r9], r10
.cC: add rsp, 16
.cD: sub rsp, 8
.cE: push 13
.cF: mov rax, constants + 749
push rax
.cG: push rsp
.cH: add qword [rsp], 16
.cI: push rsp
.cJ: add qword [rsp], 8
.cK: pop r8
.cL: pop r9
.cM: mov r10, qword [r8]
.cN: mov qword [r9], r10
.cO: add rsp, 16
.cP: mov r8, rsp
.cQ: and rsp, -16
.cR: push r8
.cS: sub rsp, 8
.cT: push 0
.cU: pop r8
.cV: sub rsp, 32
.cW: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call [GetModuleHandleA]
.cX: add rsp, 40
.cY: pop rsp
.cZ: push rax
.d0: push 0
.d1: push 0
.d2: push 0
.d3: push 0
.d4: push 0
.d5: push 0
.d6: push 0
.d7: push 0
.d8: push 0
.d9: push 0
.da: sub rsp, 8
.db: push rsp
.dc: push rbp
.dd: add qword [rsp], -24
.de: pop r8
.df: pop r9
.dg: mov r10, qword [r8]
.dh: mov qword [r9], r10
.di: push rbp
.dj: add qword [rsp], -104
.dk: add qword [rsp], 24
.dl: push rsp
.dm: add qword [rsp], 8
.dn: pop r8
.do: pop r9
.dp: mov r10, qword [r8]
.dq: mov qword [r9], r10
.dr: add rsp, 8
.ds: push 80
.dt: push rbp
.du: add qword [rsp], -104
.dv: push rsp
.dw: add qword [rsp], 8
.dx: pop r8
.dy: pop r9
.dz: mov r10d, dword [r8]
.dA: mov dword [r9], r10d
.dB: add rsp, 8
.dC: mov rax, .aJ
push rax
.dD: push rbp
.dE: add qword [rsp], -104
.dF: add qword [rsp], 8
.dG: push rsp
.dH: add qword [rsp], 8
.dI: pop r8
.dJ: pop r9
.dK: mov r10, qword [r8]
.dL: mov qword [r9], r10
.dM: add rsp, 8
.dN: sub rsp, 8
.dO: push rsp
.dP: push rbp
.dQ: add qword [rsp], -8
.dR: pop r8
.dS: pop r9
.dT: mov r10, qword [r8]
.dU: mov qword [r9], r10
.dV: push rbp
.dW: add qword [rsp], -104
.dX: add qword [rsp], 64
.dY: push rsp
.dZ: add qword [rsp], 8
.e0: pop r8
.e1: pop r9
.e2: mov r10, qword [r8]
.e3: mov qword [r9], r10
.e4: add rsp, 8
.e5: mov r8, rsp
.e6: and rsp, -16
.e7: push r8
.e8: sub rsp, 8
.e9: push 32512
.ea: push 0
.eb: pop r8
.ec: pop r9
.ed: sub rsp, 32
.ee: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call [LoadCursorA]
.ef: add rsp, 40
.eg: pop rsp
.eh: push rax
.ei: push rbp
.ej: add qword [rsp], -104
.ek: add qword [rsp], 40
.el: push rsp
.em: add qword [rsp], 8
.en: pop r8
.eo: pop r9
.ep: mov r10, qword [r8]
.eq: mov qword [r9], r10
.er: add rsp, 8
.es: mov r8, rsp
.et: and rsp, -16
.eu: push r8
.ev: sub rsp, 8
.ew: push rbp
.ex: add qword [rsp], -104
.ey: pop r8
.ez: sub rsp, 32
.eA: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call [RegisterClassExA]
.eB: add rsp, 40
.eC: pop rsp
.eD: push rax
.eE: push 0
.eF: pop r9
.eG: pop r8
.eH: xor r10, r10
cmp r8w, r9w
setne r10b
.eI: push r10
.eJ: pop r8
.eK: test r8, r8
jz .eQ
.eL: push 15
.eM: mov rax, constants + 762
push rax
.eN: call .0
.eO: add rsp, 16
.eP: jmp .eU
.eQ: push 14
.eR: mov rax, constants + 777
push rax
.eS: call .0
.eT: add rsp, 16
.eU: sub rsp, 80
.eV: push rsp
.eW: push rbp
.eX: add qword [rsp], -104
.eY: mov rcx, 80
pop rsi
pop rdi
cld
rep movsb
.eZ: mov r8, rsp
.f0: and rsp, -16
.f1: push r8
.f2: sub rsp, 8
.f3: push 0
.f4: sub rsp, 8
.f5: push rsp
.f6: push rbp
.f7: add qword [rsp], -24
.f8: pop r8
.f9: pop r9
.fa: mov r10, qword [r8]
.fb: mov qword [r9], r10
.fc: push 0
.fd: push 0
.fe: push -2147483648
.ff: push -2147483648
.fg: push -2147483648
.fh: push -2147483648
.fi: push 282001408
.fj: sub rsp, 8
.fk: push rsp
.fl: push rbp
.fm: add qword [rsp], -16
.fn: pop r8
.fo: pop r9
.fp: mov r10, qword [r8]
.fq: mov qword [r9], r10
.fr: sub rsp, 8
.fs: push rsp
.ft: push rbp
.fu: add qword [rsp], -8
.fv: pop r8
.fw: pop r9
.fx: mov r10, qword [r8]
.fy: mov qword [r9], r10
.fz: push 0
.fA: pop r8
.fB: pop r9
.fC: pop r10
.fD: pop r11
.fE: sub rsp, 32
.fF: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call [CreateWindowExA]
.fG: add rsp, 104
.fH: pop rsp
.fI: push rax
.fJ: sub rsp, 8
.fK: push rsp
.fL: push rbp
.fM: add qword [rsp], -192
.fN: pop r8
.fO: pop r9
.fP: mov r10, qword [r8]
.fQ: mov qword [r9], r10
.fR: push 0
.fS: pop r9
.fT: pop r8
.fU: xor r10, r10
cmp r8, r9
setne r10b
.fV: push r10
.fW: pop r8
.fX: test r8, r8
jz .g3
.fY: push 16
.fZ: mov rax, constants + 791
push rax
.g0: call .0
.g1: add rsp, 16
.g2: jmp .g7
.g3: push 13
.g4: mov rax, constants + 807
push rax
.g5: call .0
.g6: add rsp, 16
.g7: push 0
.g8: push 0
.g9: push 0
.ga: push 0
.gb: push 0
.gc: push 0
.gd: push 1
.ge: pop r8
.gf: test r8, r8
jz .hB
.gg: mov r8, rsp
.gh: and rsp, -16
.gi: push r8
.gj: push 1
.gk: push 0
.gl: push 0
.gm: push 0
.gn: push rbp
.go: add qword [rsp], -240
.gp: pop r8
.gq: pop r9
.gr: pop r10
.gs: pop r11
.gt: sub rsp, 32
.gu: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call [PeekMessageA]
.gv: add rsp, 40
.gw: pop rsp
.gx: push rax
.gy: push 0
.gz: pop r9
.gA: pop r8
.gB: xor r10, r10
cmp r8d, r9d
setne r10b
.gC: push r10
.gD: pop r8
.gE: test r8, r8
jz .hA
.gF: sub rsp, 8
.gG: sub rsp, 48
.gH: push rsp
.gI: push rbp
.gJ: add qword [rsp], -240
.gK: mov rcx, 48
pop rsi
pop rdi
cld
rep movsb
.gL: push rsp
.gM: add qword [rsp], 48
.gN: push rsp
.gO: add qword [rsp], 16
.gP: pop r8
.gQ: pop r9
.gR: mov r10d, dword [r8]
.gS: mov dword [r9], r10d
.gT: add rsp, 48
.gU: sub rsp, 8
.gV: push rsp
.gW: mov rax, constants + 600
push rax
.gX: pop r8
.gY: pop r9
.gZ: mov r10d, dword [r8]
.h0: mov dword [r9], r10d
.h1: pop r9
.h2: pop r8
.h3: xor r10, r10
cmp r8d, r9d
sete r10b
.h4: push r10
.h5: pop r8
.h6: test r8, r8
jz .h9
.h7: jmp .hB
.h8: jmp .h9
.h9: mov r8, rsp
.ha: and rsp, -16
.hb: push r8
.hc: sub rsp, 8
.hd: push rbp
.he: add qword [rsp], -240
.hf: pop r8
.hg: sub rsp, 32
.hh: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call [TranslateMessage]
.hi: add rsp, 40
.hj: pop rsp
.hk: push rax
.hl: add rsp, 8
.hm: mov r8, rsp
.hn: and rsp, -16
.ho: push r8
.hp: sub rsp, 8
.hq: push rbp
.hr: add qword [rsp], -240
.hs: pop r8
.ht: sub rsp, 32
.hu: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call [DispatchMessageA]
.hv: add rsp, 40
.hw: pop rsp
.hx: push rax
.hy: add rsp, 8
.hz: jmp .gg
.hA: jmp .gd
.hB: mov rsp, rbp
.hC: pop rbp
.hD: ret
section '.rodata' data readable
constants db 246,255,255,255,255,255,255,255,245,255,255,255,255,255,255,255,244,255,255,255,255,255,255,255,0,0,0,128,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,16,0,0,0,0,4,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,32,0,0,0,0,0,0,16,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,16,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,128,0,0,0,0,0,16,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,32,0,0,0,0,0,0,64,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,192,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,8,0,0,0,0,0,0,64,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,207,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,136,128,0,0,0,0,0,0,4,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,207,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,18,0,0,0,0,0,0,0,138,127,0,0,0,0,0,0,0,127,0,0,0,0,0,0,3,127,0,0,0,0,0,0,137,127,0,0,0,0,0,0,139,127,0,0,0,0,0,0,1,127,0,0,0,0,0,0,129,127,0,0,0,0,0,0,136,127,0,0,0,0,0,0,128,127,0,0,0,0,0,0,134,127,0,0,0,0,0,0,131,127,0,0,0,0,0,0,133,127,0,0,0,0,0,0,130,127,0,0,0,0,0,0,132,127,0,0,0,0,0,0,4,127,0,0,0,0,0,0,2,127,0,0,0,0,0,0,119,105,110,100,111,119,95,99,108,97,115,115,0,104,101,108,108,111,32,119,105,110,100,111,119,0,67,108,97,115,115,32,99,114,101,97,116,101,100,33,10,67,108,97,115,115,32,70,97,105,108,101,100,33,10,87,105,110,100,111,119,32,83,117,99,99,101,115,115,33,10,87,105,110,100,111,119,32,70,97,105,108,33,10
section '.idata' import data readable writeable
library user32,'user32.dll',\
	kernel32,'kernel32.dll'
import user32,\
	RegisterClassExA,'RegisterClassExA',\
	DefWindowProcA,'DefWindowProcA',\
	CreateWindowExA,'CreateWindowExA',\
	PeekMessageA,'PeekMessageA',\
	TranslateMessage,'TranslateMessage',\
	DispatchMessageA,'DispatchMessageA',\
	PostQuitMessage,'PostQuitMessage',\
	LoadCursorA,'LoadCursorA'
import kernel32,\
	GetStdHandle,'GetStdHandle',\
	WriteConsoleA,'WriteConsoleA',\
	CreateFileA,'CreateFileA',\
	WriteFile,'WriteFile',\
	GetLastError,'GetLastError',\
	VirtualAlloc,'VirtualAlloc',\
	GetModuleHandleA,'GetModuleHandleA',\
	ExitProcess,'ExitProcess'
