import "tlang/windows.tl"

#layout_c
#stdcall
#extern_library "gdi32"

HGLRC :: *Void

PIXELFORMATDESCRIPTOR :: struct {
    nSize           : WORD
    nVersion        : WORD
    dwFlags         : DWORD
    iPixelType      : BYTE
    cColorBits      : BYTE
    cRedBits        : BYTE
    cRedShift       : BYTE
    cGreenBits      : BYTE
    cGreenShift     : BYTE
    cBlueBits       : BYTE
    cBlueShift      : BYTE
    cAlphaBits      : BYTE
    cAlphaShift     : BYTE
    cAccumBits      : BYTE
    cAccumRedBits   : BYTE
    cAccumGreenBits : BYTE
    cAccumBlueBits  : BYTE
    cAccumAlphaBits : BYTE
    cDepthBits      : BYTE
    cStencilBits    : BYTE
    cAuxBuffers     : BYTE
    iLayerType      : BYTE
    bReserved       : BYTE
    dwLayerMask     : DWORD
    dwVisibleMask   : DWORD
    dwDamageMask    : DWORD
}

PFD_TYPE_RGBA       :: 0
PFD_TYPE_COLORINDEX :: 1

/* layer types */
PFD_MAIN_PLANE     :: 0
PFD_OVERLAY_PLANE  :: 1
PFD_UNDERLAY_PLANE :: -1

/* PIXELFORMATDESCRIPTOR flags */
PFD_DOUBLEBUFFER         :: 0x00000001
PFD_STEREO               :: 0x00000002
PFD_DRAW_TO_WINDOW       :: 0x00000004
PFD_DRAW_TO_BITMAP       :: 0x00000008
PFD_SUPPORT_GDI          :: 0x00000010
PFD_SUPPORT_OPENGL       :: 0x00000020
PFD_GENERIC_FORMAT       :: 0x00000040
PFD_NEED_PALETTE         :: 0x00000080
PFD_NEED_SYSTEM_PALETTE  :: 0x00000100
PFD_SWAP_EXCHANGE        :: 0x00000200
PFD_SWAP_COPY            :: 0x00000400
PFD_SWAP_LAYER_BUFFERS   :: 0x00000800
PFD_GENERIC_ACCELERATED  :: 0x00001000
PFD_SUPPORT_DIRECTDRAW   :: 0x00002000
PFD_DIRECT3D_ACCELERATED :: 0x00004000
PFD_SUPPORT_COMPOSITION  :: 0x00008000

/* PIXELFORMATDESCRIPTOR flags for use in ChoosePixelFormat only */
PFD_DEPTH_DONTCARE        :: 0x20000000
PFD_DOUBLEBUFFER_DONTCARE :: 0x40000000
PFD_STEREO_DONTCARE       :: 0x80000000

ChoosePixelFormat :: (
  hdc  : HDC,
  ppfd : *PIXELFORMATDESCRIPTOR
): INT

SwapBuffers :: (
  unnamedParam1 : HDC
): BOOL

DescribePixelFormat :: (
  hdc          : HDC,
  iPixelFormat : INT,
  nBytes       : UINT,
  ppfd         : *PIXELFORMATDESCRIPTOR
): INT

SetPixelFormat :: (
  hdc    : HDC,
  format : INT,
  ppfd   : *PIXELFORMATDESCRIPTOR
): BOOL
