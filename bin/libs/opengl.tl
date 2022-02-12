import "wingdi.tl"

#layout_c
#stdcall
#extern_library "opengl32"

wglCreateContext :: fn (
  unnamedParam1: HDC
) -> HGLRC;

wglMakeCurrent :: fn (
  unnamedParam1: HDC,
  unnamedParam2: HGLRC
) -> BOOL;
