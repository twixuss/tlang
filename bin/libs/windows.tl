INT   :: s32;
UINT  :: u32;
LONG  :: s32;
ULONG :: u32;

BOOL :: s32;

BYTE  :: u8;
WORD  :: u16;
DWORD :: u32;
QWORD :: u64;

LPBYTE  :: *BYTE;
LPWORD  :: *WORD;
LPDWORD :: *DWORD;
LPQWORD :: *QWORD;

INT_PTR :: int;
LONG_PTR :: int;
UINT_PTR :: uint;
ULONG_PTR :: uint;

SIZE_T  :: ULONG_PTR;
SSIZE_T :: LONG_PTR;

WPARAM  :: UINT_PTR;
LPARAM  :: LONG_PTR;
LRESULT :: LONG_PTR;

ATOM :: u16;

LPVOID :: *void;
LPCVOID :: *void;

HANDLE    :: *void;
HMODULE   :: *void;
HWND      :: *void;
HICON     :: *void;
HBRUSH    :: *void;
HCURSOR   :: *void;
HINSTANCE :: *void;
HMENU     :: *void;

PSTR   :: *u8;
PCSTR  :: PSTR;
LPSTR  :: PSTR;
LPCSTR :: PSTR;

WNDPROC :: #type fn #stdcall (hwnd : HWND, uMsg : UINT, wParam : WPARAM, lParam : LPARAM) -> LRESULT;

#layout_c

WNDCLASSEXA :: struct {
	cbSize        : UINT;
	style         : UINT;
	lpfnWndProc   : WNDPROC;
	cbClsExtra    : s32;
	cbWndExtra    : s32;
	hInstance     : HINSTANCE;
	hIcon         : HICON;
	hCursor       : HCURSOR;
	hbrBackground : HBRUSH;
	lpszMenuName  : LPCSTR;
	lpszClassName : LPCSTR;
	hIconSm       : HICON;
}

POINT :: struct {
  x : LONG;
  y : LONG;
}

MSG :: struct {
  hwnd     : HWND;
  message  : UINT;
  wParam   : WPARAM;
  lParam   : LPARAM;
  time     : DWORD;
  pt       : POINT;
  lPrivate : DWORD;
}

LPMSG :: *MSG;

STD_INPUT_HANDLE  :: -10; // The standard input device. Initially, this is the console input buffer, CONIN$.
STD_OUTPUT_HANDLE :: -11; // The standard output device. Initially, this is the active console screen buffer, CONOUT$.
STD_ERROR_HANDLE  :: -12; // The standard error device. Initially, this is the active console screen buffer, CONOUT$.

SECURITY_ATTRIBUTES :: void;
OVERLAPPED :: void;

GENERIC_READ    :: 0x80000000;
GENERIC_WRITE   :: 0x40000000;
GENERIC_EXECUTE :: 0x20000000;
GENERIC_ALL     :: 0x10000000;

FILE_SHARE_DELETE :: 0x00000004;
FILE_SHARE_READ   :: 0x00000001;
FILE_SHARE_WRITE  :: 0x00000002;

CREATE_ALWAYS     :: 2;
CREATE_NEW        :: 1;
OPEN_ALWAYS       :: 4;
OPEN_EXISTING     :: 3;
TRUNCATE_EXISTING :: 5;

FILE_ATTRIBUTE_READONLY      :: 0x00000001;
FILE_ATTRIBUTE_HIDDEN        :: 0x00000002;
FILE_ATTRIBUTE_SYSTEM        :: 0x00000004;
FILE_ATTRIBUTE_ARCHIVE       :: 0x00000020;
FILE_ATTRIBUTE_NORMAL        :: 0x00000080;
FILE_ATTRIBUTE_TEMPORARY     :: 0x00000100;
FILE_ATTRIBUTE_OFFLINE       :: 0x00001000;
FILE_ATTRIBUTE_ENCRYPTED     :: 0x00004000;
FILE_FLAG_BACKUP_SEMANTICS   :: 0x02000000;
FILE_FLAG_DELETE_ON_CLOSE    :: 0x04000000;
FILE_FLAG_NO_BUFFERING       :: 0x20000000;
FILE_FLAG_OPEN_NO_RECALL     :: 0x00100000;
FILE_FLAG_OPEN_REPARSE_POINT :: 0x00200000;
FILE_FLAG_OVERLAPPED         :: 0x40000000;
FILE_FLAG_POSIX_SEMANTICS    :: 0x01000000;
FILE_FLAG_RANDOM_ACCESS      :: 0x10000000;
FILE_FLAG_SESSION_AWARE      :: 0x00800000;
FILE_FLAG_SEQUENTIAL_SCAN    :: 0x08000000;
FILE_FLAG_WRITE_THROUGH      :: 0x80000000;

MEM_COMMIT      :: 0x00001000;
MEM_RESERVE     :: 0x00002000;
MEM_RESET       :: 0x00080000;
MEM_RESET_UNDO  :: 0x01000000;
MEM_LARGE_PAGES :: 0x20000000;
MEM_PHYSICAL    :: 0x00400000;
MEM_TOP_DOWN    :: 0x00100000;
MEM_WRITE_WATCH :: 0x00200000;

WS_BORDER           :: 0x00800000;
WS_CAPTION          :: 0x00C00000;
WS_CHILD            :: 0x40000000;
WS_CHILDWINDOW      :: 0x40000000;
WS_CLIPCHILDREN     :: 0x02000000;
WS_CLIPSIBLINGS     :: 0x04000000;
WS_DISABLED         :: 0x08000000;
WS_DLGFRAME         :: 0x00400000;
WS_GROUP            :: 0x00020000;
WS_HSCROLL          :: 0x00100000;
WS_ICONIC           :: 0x20000000;
WS_MAXIMIZE         :: 0x01000000;
WS_MAXIMIZEBOX      :: 0x00010000;
WS_MINIMIZE         :: 0x20000000;
WS_MINIMIZEBOX      :: 0x00020000;
WS_OVERLAPPED       :: 0x00000000;
WS_OVERLAPPEDWINDOW :: WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX;
WS_POPUP            :: 0x80000000;
WS_POPUPWINDOW      :: WS_POPUP | WS_BORDER | WS_SYSMENU;
WS_SIZEBOX          :: 0x00040000;
WS_SYSMENU          :: 0x00080000;
WS_TABSTOP          :: 0x00010000;
WS_THICKFRAME       :: 0x00040000;
WS_TILED            :: 0x00000000;
WS_TILEDWINDOW      :: WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX;
WS_VISIBLE          :: 0x10000000;
WS_VSCROLL          :: 0x00200000;

CW_USEDEFAULT :: 0x80000000;

PM_NOREMOVE :: 0x0000;
PM_REMOVE   :: 0x0001;
PM_NOYIELD  :: 0x0002;

WM_DESTROY :: 0x0002;
WM_CLOSE   :: 0x0010;
WM_QUIT    :: 0x0012;


IDC_APPSTARTING :: 32650 as *u8;
IDC_ARROW       :: 32512 as *u8;
IDC_CROSS       :: 32515 as *u8;
IDC_HAND        :: 32649 as *u8;
IDC_HELP        :: 32651 as *u8;
IDC_IBEAM       :: 32513 as *u8;
IDC_ICON        :: 32641 as *u8;
IDC_NO          :: 32648 as *u8;
IDC_SIZE        :: 32640 as *u8;
IDC_SIZEALL     :: 32646 as *u8;
IDC_SIZENESW    :: 32643 as *u8;
IDC_SIZENS      :: 32645 as *u8;
IDC_SIZENWSE    :: 32642 as *u8;
IDC_SIZEWE      :: 32644 as *u8;
IDC_UPARROW     :: 32516 as *u8;
IDC_WAIT        :: 32514 as *u8;


#stdcall
#extern_library "kernel32.lib"

GetStdHandle :: fn (nStdHandle: DWORD) -> HANDLE;

WriteConsoleA :: fn (hConsoleOutput: HANDLE, lpBuffer: LPCVOID, nNumberOfCharsToWrite: DWORD, lpNumberOfCharsWritten: LPDWORD, lpReserved: LPVOID) -> BOOL;

CreateFileA :: fn (
	lpFileName: LPCSTR,
	dwDesiredAccess: DWORD,
	dwShareMode: DWORD,
	lpSecurityAttributes: *SECURITY_ATTRIBUTES,
	dwCreationDisposition: DWORD,
	dwFlagsAndAttributes: DWORD,
	hTemplateFile: HANDLE
) -> HANDLE;

WriteFile :: fn (
	hFile:                  HANDLE,
	lpBuffer:               LPCVOID,
	nNumberOfBytesToWrite:  DWORD,
	lpNumberOfBytesWritten: LPDWORD,
	lpOverlapped:           *OVERLAPPED
) -> BOOL;

GetLastError :: fn () -> DWORD;

VirtualAlloc :: fn (
	lpAddress        : LPVOID,
	dwSize           : SIZE_T,
	flAllocationType : DWORD,
	flProtect        : DWORD
) -> LPVOID;

GetModuleHandleA :: fn (
	lpModuleName : LPCSTR
) -> HMODULE;

#extern_library "user32.lib"

RegisterClassExA :: fn (
	unnamedParam1 : *WNDCLASSEXA
) -> ATOM;

DefWindowProcA :: fn (
	hWnd   : HWND,
	Msg    : UINT,
	wParam : WPARAM,
	lParam : LPARAM 
) -> LRESULT ;

CreateWindowExA :: fn (
	dwExStyle    : DWORD,
	lpClassName  : LPCSTR,
	lpWindowName : LPCSTR,
	dwStyle      : DWORD,
	X            : s32,
	Y            : s32,
	nWidth       : s32,
	nHeight      : s32,
	hWndParent   : HWND,
	hMenu        : HMENU,
	hInstance    : HINSTANCE,
	lpParam      : LPVOID
) -> HWND;

PeekMessageA :: fn (
	lpMsg         : LPMSG,
	hWnd          : HWND,
	wMsgFilterMin : UINT, 
	wMsgFilterMax : UINT, 
	wRemoveMsg    : UINT 
) -> BOOL;

TranslateMessage :: fn (
	lpMsg : *MSG
) -> BOOL;

DispatchMessageA :: fn (
	lpMsg : *MSG
) -> LRESULT;

PostQuitMessage :: fn (
  nExitCode : s32
);

LoadCursorA :: fn (
  hInstance    : HINSTANCE,
  lpCursorName : LPCSTR
) -> HCURSOR;