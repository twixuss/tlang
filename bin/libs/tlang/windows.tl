#stdcall

SHORT :: S16
INT   :: S32
LONG  :: S32

USHORT :: U16
UINT   :: U32
ULONG  :: U32

PUINT :: *U32

BOOL :: S32

BYTE  :: U8
WORD  :: U16
DWORD :: U32
QWORD :: U64

LPBYTE  :: *BYTE
LPWORD  :: *WORD
LPDWORD :: *DWORD
LPQWORD :: *QWORD

INT_PTR   :: Int
LONG_PTR  :: Int
UINT_PTR  :: UInt
ULONG_PTR :: UInt

SIZE_T  :: ULONG_PTR
SSIZE_T :: LONG_PTR

WPARAM  :: UINT_PTR
LPARAM  :: LONG_PTR
LRESULT :: LONG_PTR

ATOM :: U16

PVOID   :: *Void
LPVOID  :: *Void
LPCVOID :: *Void

HANDLE    :: *Void
HMODULE   :: *Void
HWND      :: *Void
HICON     :: *Void
HBRUSH    :: *Void
HCURSOR   :: *Void
HINSTANCE :: *Void
HMENU     :: *Void
HDC       :: *Void
HRAWINPUT :: *Void

INVALID_HANDLE_VALUE :: -1 as HANDLE

PSTR   :: *U8
PCSTR  :: PSTR
LPSTR  :: PSTR
LPCSTR :: PSTR

WNDPROC :: (hwnd : HWND, uMsg : UINT, wParam : WPARAM, lParam : LPARAM) LRESULT #type

LARGE_INTEGER :: S64
PLARGE_INTEGER :: *LARGE_INTEGER

FILE_BEGIN   :: 0 // The starting point is zero or the beginning of the file. If this flag is specified, then the liDistanceToMove parameter is interpreted as an unsigned value.
FILE_CURRENT :: 1 // The start point is the current value of the file pointer.
FILE_END     :: 2 // The starting point is the current end-of-file position.

#layout_c

WNDCLASSEXA :: struct {
	cbSize        : UINT
	style         : UINT
	lpfnWndProc   : WNDPROC
	cbClsExtra    : INT
	cbWndExtra    : INT
	hInstance     : HINSTANCE
	hIcon         : HICON
	hCursor       : HCURSOR
	hbrBackground : HBRUSH
	lpszMenuName  : LPCSTR
	lpszClassName : LPCSTR
	hIconSm       : HICON
}

POINT :: struct {
  x : LONG
  y : LONG
}

MSG :: struct {
  hwnd     : HWND
  message  : UINT
  wParam   : WPARAM
  lParam   : LPARAM
  time     : DWORD
  pt       : POINT
  lPrivate : DWORD
}
LPMSG :: *MSG

RAWINPUTDEVICE :: struct {
    usUsagePage: USHORT // Toplevel collection UsagePage
    usUsage:     USHORT // Toplevel collection Usage
    dwFlags:     DWORD
    hwndTarget:  HWND   // Target hwnd. NULL = follows keyboard focus
}
PCRAWINPUTDEVICE :: *RAWINPUTDEVICE

/*
 * Struct pointed to by WM_GETMINMAXINFO lParam
 */
MINMAXINFO :: struct {
    ptReserved:     POINT
    ptMaxSize:      POINT
    ptMaxPosition:  POINT
    ptMinTrackSize: POINT
    ptMaxTrackSize: POINT
}
PMINMAXINFO  :: *MINMAXINFO
LPMINMAXINFO :: *MINMAXINFO


RAWINPUTHEADER :: struct {
    dwType:  DWORD
    dwSize:  DWORD
    hDevice: HANDLE
    wParam:  WPARAM
}
PRAWINPUTHEADER  :: *RAWINPUTHEADER
LPRAWINPUTHEADER :: *RAWINPUTHEADER


RAWMOUSE :: struct {
  usFlags: USHORT
  u : union { // TODO: unnamed structs and unions
    ulButtons: ULONG
    s : struct {
      usButtonFlags: USHORT
      usButtonData: USHORT
    }
  }
  ulRawButtons:       ULONG
  lLastX:			  LONG
  lLastY:			  LONG
  ulExtraInformation: ULONG
}
PRAWMOUSE  :: *RAWMOUSE
LPRAWMOUSE :: *RAWMOUSE


RAWHID :: struct {
  dwSizeHid: DWORD
  dwCount: DWORD
}
PRAWHID  :: *RAWHID
LPRAWHID :: *RAWHID


RAWKEYBOARD :: struct {
  MakeCode:         USHORT
  Flags:            USHORT
  Reserved:         USHORT
  VKey:             USHORT
  Message:          UINT
  ExtraInformation: ULONG
}
PRAWKEYBOARD  :: *RAWKEYBOARD
LPRAWKEYBOARD :: *RAWKEYBOARD


RAWINPUT :: struct {
	header: RAWINPUTHEADER
	data : union {
		mouse:    RAWMOUSE
		keyboard: RAWKEYBOARD
		hid:      RAWHID
	}
}
PRAWINPUT  :: *RAWINPUT
LPRAWINPUT :: *RAWINPUT

STD_INPUT_HANDLE  :: -10 as U32 // The standard input device. Initially, this is the console input buffer, CONIN$.
STD_OUTPUT_HANDLE :: -11 as U32 // The standard output device. Initially, this is the active console screen buffer, CONOUT$.
STD_ERROR_HANDLE  :: -12 as U32 // The standard error device. Initially, this is the active console screen buffer, CONOUT$.

SECURITY_ATTRIBUTES :: Void

OVERLAPPED :: struct {
	Internal: ULONG_PTR
	InternalHigh: ULONG_PTR
	DUMMYUNIONNAME: union {
		DUMMYSTRUCTNAME: struct {
			Offset: DWORD
			OffsetHigh: DWORD
		}
		Pointer: PVOID
	}
	hEvent: HANDLE
}

LPOVERLAPPED :: *OVERLAPPED

GENERIC_READ    :: 0x80000000
GENERIC_WRITE   :: 0x40000000
GENERIC_EXECUTE :: 0x20000000
GENERIC_ALL     :: 0x10000000

FILE_SHARE_DELETE :: 0x00000004
FILE_SHARE_READ   :: 0x00000001
FILE_SHARE_WRITE  :: 0x00000002

CREATE_ALWAYS     :: 2
CREATE_NEW        :: 1
OPEN_ALWAYS       :: 4
OPEN_EXISTING     :: 3
TRUNCATE_EXISTING :: 5

FILE_ATTRIBUTE_READONLY      :: 0x00000001
FILE_ATTRIBUTE_HIDDEN        :: 0x00000002
FILE_ATTRIBUTE_SYSTEM        :: 0x00000004
FILE_ATTRIBUTE_ARCHIVE       :: 0x00000020
FILE_ATTRIBUTE_NORMAL        :: 0x00000080
FILE_ATTRIBUTE_TEMPORARY     :: 0x00000100
FILE_ATTRIBUTE_OFFLINE       :: 0x00001000
FILE_ATTRIBUTE_ENCRYPTED     :: 0x00004000
FILE_FLAG_BACKUP_SEMANTICS   :: 0x02000000
FILE_FLAG_DELETE_ON_CLOSE    :: 0x04000000
FILE_FLAG_NO_BUFFERING       :: 0x20000000
FILE_FLAG_OPEN_NO_RECALL     :: 0x00100000
FILE_FLAG_OPEN_REPARSE_POINT :: 0x00200000
FILE_FLAG_OVERLAPPED         :: 0x40000000
FILE_FLAG_POSIX_SEMANTICS    :: 0x01000000
FILE_FLAG_RANDOM_ACCESS      :: 0x10000000
FILE_FLAG_SESSION_AWARE      :: 0x00800000
FILE_FLAG_SEQUENTIAL_SCAN    :: 0x08000000
FILE_FLAG_WRITE_THROUGH      :: 0x80000000

PAGE_NOACCESS                   :: 0x00000001
PAGE_READONLY                   :: 0x00000002
PAGE_READWRITE                  :: 0x00000004
PAGE_WRITECOPY                  :: 0x00000008
PAGE_EXECUTE                    :: 0x00000010
PAGE_EXECUTE_READ               :: 0x00000020
PAGE_EXECUTE_READWRITE          :: 0x00000040
PAGE_EXECUTE_WRITECOPY          :: 0x00000080
PAGE_GUARD                      :: 0x00000100
PAGE_NOCACHE                    :: 0x00000200
PAGE_WRITECOMBINE               :: 0x00000400
PAGE_GRAPHICS_NOACCESS          :: 0x00000800
PAGE_GRAPHICS_READONLY          :: 0x00001000
PAGE_GRAPHICS_READWRITE         :: 0x00002000
PAGE_GRAPHICS_EXECUTE           :: 0x00004000
PAGE_GRAPHICS_EXECUTE_READ      :: 0x00008000
PAGE_GRAPHICS_EXECUTE_READWRITE :: 0x00010000
PAGE_GRAPHICS_COHERENT          :: 0x00020000
PAGE_ENCLAVE_THREAD_CONTROL     :: 0x80000000
PAGE_REVERT_TO_FILE_MAP         :: 0x80000000
PAGE_TARGETS_NO_UPDATE          :: 0x40000000
PAGE_TARGETS_INVALID            :: 0x40000000
PAGE_ENCLAVE_UNVALIDATED        :: 0x20000000
PAGE_ENCLAVE_DECOMMIT           :: 0x10000000

MEM_COMMIT                     :: 0x00001000
MEM_RESERVE                    :: 0x00002000
MEM_REPLACE_PLACEHOLDER        :: 0x00004000
MEM_RESERVE_PLACEHOLDER        :: 0x00040000
MEM_RESET                      :: 0x00080000
MEM_TOP_DOWN                   :: 0x00100000
MEM_WRITE_WATCH                :: 0x00200000
MEM_PHYSICAL                   :: 0x00400000
MEM_ROTATE                     :: 0x00800000
MEM_DIFFERENT_IMAGE_BASE_OK    :: 0x00800000
MEM_RESET_UNDO                 :: 0x01000000
MEM_LARGE_PAGES                :: 0x20000000
MEM_4MB_PAGES                  :: 0x80000000
MEM_64K_PAGES                  :: MEM_LARGE_PAGES | MEM_PHYSICAL
MEM_UNMAP_WITH_TRANSIENT_BOOST :: 0x00000001
MEM_COALESCE_PLACEHOLDERS      :: 0x00000001
MEM_PRESERVE_PLACEHOLDER       :: 0x00000002
MEM_DECOMMIT                   :: 0x00004000
MEM_RELEASE                    :: 0x00008000
MEM_FREE                       :: 0x00010000

WS_BORDER           :: 0x00800000
WS_CAPTION          :: 0x00C00000
WS_CHILD            :: 0x40000000
WS_CHILDWINDOW      :: 0x40000000
WS_CLIPCHILDREN     :: 0x02000000
WS_CLIPSIBLINGS     :: 0x04000000
WS_DISABLED         :: 0x08000000
WS_DLGFRAME         :: 0x00400000
WS_GROUP            :: 0x00020000
WS_HSCROLL          :: 0x00100000
WS_ICONIC           :: 0x20000000
WS_MAXIMIZE         :: 0x01000000
WS_MAXIMIZEBOX      :: 0x00010000
WS_MINIMIZE         :: 0x20000000
WS_MINIMIZEBOX      :: 0x00020000
WS_OVERLAPPED       :: 0x00000000
WS_OVERLAPPEDWINDOW :: WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX
WS_POPUP            :: 0x80000000
WS_POPUPWINDOW      :: WS_POPUP | WS_BORDER | WS_SYSMENU
WS_SIZEBOX          :: 0x00040000
WS_SYSMENU          :: 0x00080000
WS_TABSTOP          :: 0x00010000
WS_THICKFRAME       :: 0x00040000
WS_TILED            :: 0x00000000
WS_TILEDWINDOW      :: WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX
WS_VISIBLE          :: 0x10000000
WS_VSCROLL          :: 0x00200000

CW_USEDEFAULT :: 0x80000000

PM_NOREMOVE :: 0x0000
PM_REMOVE   :: 0x0001
PM_NOYIELD  :: 0x0002

WINVER  :: 0x0500

/*
 * Window Messages
 */

WM_NULL                         :: 0x0000
WM_CREATE                       :: 0x0001
WM_DESTROY                      :: 0x0002
WM_MOVE                         :: 0x0003
WM_SIZE                         :: 0x0005
WM_ACTIVATE                     :: 0x0006

/*
 * WM_ACTIVATE state values
 */
WA_INACTIVE     :: 0
WA_ACTIVE       :: 1
WA_CLICKACTIVE  :: 2

WM_SETFOCUS                     :: 0x0007
WM_KILLFOCUS                    :: 0x0008
WM_ENABLE                       :: 0x000A
WM_SETREDRAW                    :: 0x000B
WM_SETTEXT                      :: 0x000C
WM_GETTEXT                      :: 0x000D
WM_GETTEXTLENGTH                :: 0x000E
WM_PAINT                        :: 0x000F
WM_CLOSE                        :: 0x0010
WM_QUIT                         :: 0x0012
WM_ERASEBKGND                   :: 0x0014
WM_SYSCOLORCHANGE               :: 0x0015
WM_SHOWWINDOW                   :: 0x0018
WM_WININICHANGE                 :: 0x001A

#if WINVER >= 0x0400 {
	WM_SETTINGCHANGE                :: WM_WININICHANGE
}

WM_DEVMODECHANGE                :: 0x001B
WM_ACTIVATEAPP                  :: 0x001C
WM_FONTCHANGE                   :: 0x001D
WM_TIMECHANGE                   :: 0x001E
WM_CANCELMODE                   :: 0x001F
WM_SETCURSOR                    :: 0x0020
WM_MOUSEACTIVATE                :: 0x0021
WM_CHILDACTIVATE                :: 0x0022
WM_QUEUESYNC                    :: 0x0023

WM_GETMINMAXINFO                :: 0x0024

WM_PAINTICON                    :: 0x0026
WM_ICONERASEBKGND               :: 0x0027
WM_NEXTDLGCTL                   :: 0x0028
WM_SPOOLERSTATUS                :: 0x002A
WM_DRAWITEM                     :: 0x002B
WM_MEASUREITEM                  :: 0x002C
WM_DELETEITEM                   :: 0x002D
WM_VKEYTOITEM                   :: 0x002E
WM_CHARTOITEM                   :: 0x002F
WM_SETFONT                      :: 0x0030
WM_GETFONT                      :: 0x0031
WM_SETHOTKEY                    :: 0x0032
WM_GETHOTKEY                    :: 0x0033
WM_QUERYDRAGICON                :: 0x0037
WM_COMPAREITEM                  :: 0x0039
#if WINVER >= 0x0500 {
	WM_GETOBJECT                    :: 0x003D
}
WM_COMPACTING                   :: 0x0041
WM_COMMNOTIFY                   :: 0x0044  /* no longer suported */
WM_WINDOWPOSCHANGING            :: 0x0046
WM_WINDOWPOSCHANGED             :: 0x0047

WM_POWER                        :: 0x0048
/*
 * wParam for WM_POWER window message and DRV_POWER driver notification
 */
PWR_OK              :: 1
PWR_FAIL            :: -1
PWR_SUSPENDREQUEST  :: 1
PWR_SUSPENDRESUME   :: 2
PWR_CRITICALRESUME  :: 3

WM_COPYDATA                     :: 0x004A
WM_CANCELJOURNAL                :: 0x004B

/*
#if true { // WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)

	/*
	* lParam of WM_COPYDATA message points to...
	*/
	COPYDATASTRUCT :: struct {
		dwData: ULONG_PTR
		cbData: DWORD
		lpData: PVOID
	}
	PCOPYDATASTRUCT :: COPYDATASTRUCT

	#if WINVER >= 0x0400 {
		MDINEXTMENU :: struct {
			hmenuIn:   HMENU
			hmenuNext: HMENU
			hwndNext:  HWND
		}
		PMDINEXTMENU :: MDINEXTMENU
		LPMDINEXTMENU :: MDINEXTMENU
	}
}
*/


#if WINVER >= 0x0400 {
	WM_NOTIFY                       :: 0x004E
	WM_INPUTLANGCHANGEREQUEST       :: 0x0050
	WM_INPUTLANGCHANGE              :: 0x0051
	WM_TCARD                        :: 0x0052
	WM_HELP                         :: 0x0053
	WM_USERCHANGED                  :: 0x0054
	WM_NOTIFYFORMAT                 :: 0x0055

	NFR_ANSI                             :: 1
	NFR_UNICODE                          :: 2
	NF_QUERY                             :: 3
	NF_REQUERY                           :: 4

	WM_CONTEXTMENU                  :: 0x007B
	WM_STYLECHANGING                :: 0x007C
	WM_STYLECHANGED                 :: 0x007D
	WM_DISPLAYCHANGE                :: 0x007E
	WM_GETICON                      :: 0x007F
	WM_SETICON                      :: 0x0080
}

WM_NCCREATE                     :: 0x0081
WM_NCDESTROY                    :: 0x0082
WM_NCCALCSIZE                   :: 0x0083
WM_NCHITTEST                    :: 0x0084
WM_NCPAINT                      :: 0x0085
WM_NCACTIVATE                   :: 0x0086
WM_GETDLGCODE                   :: 0x0087
// #ifndef _WIN32_WCE
WM_SYNCPAINT                    :: 0x0088
// #endif
WM_NCMOUSEMOVE                  :: 0x00A0
WM_NCLBUTTONDOWN                :: 0x00A1
WM_NCLBUTTONUP                  :: 0x00A2
WM_NCLBUTTONDBLCLK              :: 0x00A3
WM_NCRBUTTONDOWN                :: 0x00A4
WM_NCRBUTTONUP                  :: 0x00A5
WM_NCRBUTTONDBLCLK              :: 0x00A6
WM_NCMBUTTONDOWN                :: 0x00A7
WM_NCMBUTTONUP                  :: 0x00A8
WM_NCMBUTTONDBLCLK              :: 0x00A9



// #if _WIN32_WINNT >= 0x0500 {
WM_NCXBUTTONDOWN                :: 0x00AB
WM_NCXBUTTONUP                  :: 0x00AC
WM_NCXBUTTONDBLCLK              :: 0x00AD
// }


// #if(_WIN32_WINNT >= 0x0501)
WM_INPUT_DEVICE_CHANGE          :: 0x00FE
// #endif /* _WIN32_WINNT >= 0x0501 */

// #if(_WIN32_WINNT >= 0x0501)
WM_INPUT                        :: 0x00FF
// #endif /* _WIN32_WINNT >= 0x0501 */

WM_KEYFIRST                     :: 0x0100
WM_KEYDOWN                      :: 0x0100
WM_KEYUP                        :: 0x0101
WM_CHAR                         :: 0x0102
WM_DEADCHAR                     :: 0x0103
WM_SYSKEYDOWN                   :: 0x0104
WM_SYSKEYUP                     :: 0x0105
WM_SYSCHAR                      :: 0x0106
WM_SYSDEADCHAR                  :: 0x0107
// #if(_WIN32_WINNT >= 0x0501)
WM_UNICHAR                      :: 0x0109
WM_KEYLAST                      :: 0x0109
UNICODE_NOCHAR                  :: 0xFFFF
// #else
//WM_KEYLAST                      :: 0x0108
// #endif /* _WIN32_WINNT >= 0x0501 */

// #if(WINVER >= 0x0400)
WM_IME_STARTCOMPOSITION         :: 0x010D
WM_IME_ENDCOMPOSITION           :: 0x010E
WM_IME_COMPOSITION              :: 0x010F
WM_IME_KEYLAST                  :: 0x010F
// #endif /* WINVER >= 0x0400 */

WM_INITDIALOG                   :: 0x0110
WM_COMMAND                      :: 0x0111
WM_SYSCOMMAND                   :: 0x0112
WM_TIMER                        :: 0x0113
WM_HSCROLL                      :: 0x0114
WM_VSCROLL                      :: 0x0115
WM_INITMENU                     :: 0x0116
WM_INITMENUPOPUP                :: 0x0117
// #if(WINVER >= 0x0601)
WM_GESTURE                      :: 0x0119
WM_GESTURENOTIFY                :: 0x011A
// #endif /* WINVER >= 0x0601 */
WM_MENUSELECT                   :: 0x011F
WM_MENUCHAR                     :: 0x0120
WM_ENTERIDLE                    :: 0x0121
// #if(WINVER >= 0x0500)
// #ifndef _WIN32_WCE
WM_MENURBUTTONUP                :: 0x0122
WM_MENUDRAG                     :: 0x0123
WM_MENUGETOBJECT                :: 0x0124
WM_UNINITMENUPOPUP              :: 0x0125
WM_MENUCOMMAND                  :: 0x0126

// #ifndef _WIN32_WCE
// #if(_WIN32_WINNT >= 0x0500)
WM_CHANGEUISTATE                :: 0x0127
WM_UPDATEUISTATE                :: 0x0128
WM_QUERYUISTATE                 :: 0x0129

/*
 * LOWORD(wParam) values in WM_*UISTATE*
 */
UIS_SET                         :: 1
UIS_CLEAR                       :: 2
UIS_INITIALIZE                  :: 3

/*
 * HIWORD(wParam) values in WM_*UISTATE*
 */
UISF_HIDEFOCUS                  :: 0x1
UISF_HIDEACCEL                  :: 0x2
// #if(_WIN32_WINNT >= 0x0501)
UISF_ACTIVE                     :: 0x4
// #endif /* _WIN32_WINNT >= 0x0501 */
// #endif /* _WIN32_WINNT >= 0x0500 */
// #endif

// #endif
// #endif /* WINVER >= 0x0500 */

WM_CTLCOLORMSGBOX               :: 0x0132
WM_CTLCOLOREDIT                 :: 0x0133
WM_CTLCOLORLISTBOX              :: 0x0134
WM_CTLCOLORBTN                  :: 0x0135
WM_CTLCOLORDLG                  :: 0x0136
WM_CTLCOLORSCROLLBAR            :: 0x0137
WM_CTLCOLORSTATIC               :: 0x0138
MN_GETHMENU                     :: 0x01E1

WM_MOUSEFIRST                   :: 0x0200
WM_MOUSEMOVE                    :: 0x0200
WM_LBUTTONDOWN                  :: 0x0201
WM_LBUTTONUP                    :: 0x0202
WM_LBUTTONDBLCLK                :: 0x0203
WM_RBUTTONDOWN                  :: 0x0204
WM_RBUTTONUP                    :: 0x0205
WM_RBUTTONDBLCLK                :: 0x0206
WM_MBUTTONDOWN                  :: 0x0207
WM_MBUTTONUP                    :: 0x0208
WM_MBUTTONDBLCLK                :: 0x0209
// #if (_WIN32_WINNT >= 0x0400) || (_WIN32_WINDOWS > 0x0400)
WM_MOUSEWHEEL                   :: 0x020A
// #endif
// #if (_WIN32_WINNT >= 0x0500)
WM_XBUTTONDOWN                  :: 0x020B
WM_XBUTTONUP                    :: 0x020C
WM_XBUTTONDBLCLK                :: 0x020D
// #endif
// #if (_WIN32_WINNT >= 0x0600)
WM_MOUSEHWHEEL                  :: 0x020E
// #endif

// #if (_WIN32_WINNT >= 0x0600)
WM_MOUSELAST                    :: 0x020E
// #elif (_WIN32_WINNT >= 0x0500)
//WM_MOUSELAST                    :: 0x020D
// #elif (_WIN32_WINNT >= 0x0400) || (_WIN32_WINDOWS > 0x0400)
//WM_MOUSELAST                    :: 0x020A
// #else
//WM_MOUSELAST                    :: 0x0209
// #endif /* (_WIN32_WINNT >= 0x0600) */


// #if(_WIN32_WINNT >= 0x0400)
/* Value for rolling one detent */
WHEEL_DELTA                     :: 120
// GET_WHEEL_DELTA_WPARAM(wParam)  ((short)HIWORD(wParam:: ))

/* Setting to scroll one page for SPI_GET/SETWHEELSCROLLLINES */
// WHEEL_PAGESCROLL                (UINT_MAX:: )
// #endif /* _WIN32_WINNT >= 0x0400 */

// #if(_WIN32_WINNT >= 0x0500)
 //GET_KEYSTATE_WPARAM(wParam)     (LOWORD(wParam:: ))
 //GET_NCHITTEST_WPARAM(wParam)    ((short)LOWORD(wParam:: ))
 //GET_XBUTTON_WPARAM(wParam)      (HIWORD(wParam:: ))

/* XButton values are WORD flags */
XBUTTON1      :: 0x0001
XBUTTON2      :: 0x0002
/* Were there to be an XBUTTON3, its value would be 0x0004 */
// #endif /* _WIN32_WINNT >= 0x0500 */

WM_PARENTNOTIFY                 :: 0x0210
WM_ENTERMENULOOP                :: 0x0211
WM_EXITMENULOOP                 :: 0x0212

// #if(WINVER >= 0x0400)
WM_NEXTMENU                     :: 0x0213
WM_SIZING                       :: 0x0214
WM_CAPTURECHANGED               :: 0x0215
WM_MOVING                       :: 0x0216
// #endif /* WINVER >= 0x0400 */

// #if(WINVER >= 0x0400)


WM_POWERBROADCAST               :: 0x0218

// #ifndef _WIN32_WCE
PBT_APMQUERYSUSPEND             :: 0x0000
PBT_APMQUERYSTANDBY             :: 0x0001

PBT_APMQUERYSUSPENDFAILED       :: 0x0002
PBT_APMQUERYSTANDBYFAILED       :: 0x0003

PBT_APMSUSPEND                  :: 0x0004
PBT_APMSTANDBY                  :: 0x0005

PBT_APMRESUMECRITICAL           :: 0x0006
PBT_APMRESUMESUSPEND            :: 0x0007
PBT_APMRESUMESTANDBY            :: 0x0008

PBTF_APMRESUMEFROMFAILURE       :: 0x00000001

PBT_APMBATTERYLOW               :: 0x0009
PBT_APMPOWERSTATUSCHANGE        :: 0x000A

PBT_APMOEMEVENT                 :: 0x000B


PBT_APMRESUMEAUTOMATIC          :: 0x0012
// #if (_WIN32_WINNT >= 0x0502)
// #ifndef PBT_POWERSETTINGCHANGE
PBT_POWERSETTINGCHANGE          :: 0x8013

// #pragma region Desktop Family
// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)

/*
POWERBROADCAST_SETTING :: struct {
    PowerSetting: GUID
    DataLength: DWORD
}
PPOWERBROADCAST_SETTING :: *POWERBROADCAST_SETTING
*/

// #endif /* WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP) */
// #pragma endregion

// #endif // PBT_POWERSETTINGCHANGE

// #endif // (_WIN32_WINNT >= 0x0502)
// #endif

// #endif /* WINVER >= 0x0400 */

// #if(WINVER >= 0x0400)
WM_DEVICECHANGE                 :: 0x0219
// #endif /* WINVER >= 0x0400 */

WM_MDICREATE                    :: 0x0220
WM_MDIDESTROY                   :: 0x0221
WM_MDIACTIVATE                  :: 0x0222
WM_MDIRESTORE                   :: 0x0223
WM_MDINEXT                      :: 0x0224
WM_MDIMAXIMIZE                  :: 0x0225
WM_MDITILE                      :: 0x0226
WM_MDICASCADE                   :: 0x0227
WM_MDIICONARRANGE               :: 0x0228
WM_MDIGETACTIVE                 :: 0x0229


WM_MDISETMENU                   :: 0x0230
WM_ENTERSIZEMOVE                :: 0x0231
WM_EXITSIZEMOVE                 :: 0x0232
WM_DROPFILES                    :: 0x0233
WM_MDIREFRESHMENU               :: 0x0234

// #if(WINVER >= 0x0602)
WM_POINTERDEVICECHANGE          :: 0x238
WM_POINTERDEVICEINRANGE         :: 0x239
WM_POINTERDEVICEOUTOFRANGE      :: 0x23A
// #endif /* WINVER >= 0x0602 */


// #if(WINVER >= 0x0601)
WM_TOUCH                        :: 0x0240
// #endif /* WINVER >= 0x0601 */

// #if(WINVER >= 0x0602)
WM_NCPOINTERUPDATE              :: 0x0241
WM_NCPOINTERDOWN                :: 0x0242
WM_NCPOINTERUP                  :: 0x0243
WM_POINTERUPDATE                :: 0x0245
WM_POINTERDOWN                  :: 0x0246
WM_POINTERUP                    :: 0x0247
WM_POINTERENTER                 :: 0x0249
WM_POINTERLEAVE                 :: 0x024A
WM_POINTERACTIVATE              :: 0x024B
WM_POINTERCAPTURECHANGED        :: 0x024C
WM_TOUCHHITTESTING              :: 0x024D
WM_POINTERWHEEL                 :: 0x024E
WM_POINTERHWHEEL                :: 0x024F
DM_POINTERHITTEST               :: 0x0250
WM_POINTERROUTEDTO              :: 0x0251
WM_POINTERROUTEDAWAY            :: 0x0252
WM_POINTERROUTEDRELEASED        :: 0x0253
// #endif /* WINVER >= 0x0602 */


// #if(WINVER >= 0x0400)
WM_IME_SETCONTEXT               :: 0x0281
WM_IME_NOTIFY                   :: 0x0282
WM_IME_CONTROL                  :: 0x0283
WM_IME_COMPOSITIONFULL          :: 0x0284
WM_IME_SELECT                   :: 0x0285
WM_IME_CHAR                     :: 0x0286
// #endif /* WINVER >= 0x0400 */
// #if(WINVER >= 0x0500)
WM_IME_REQUEST                  :: 0x0288
// #endif /* WINVER >= 0x0500 */
// #if(WINVER >= 0x0400)
WM_IME_KEYDOWN                  :: 0x0290
WM_IME_KEYUP                    :: 0x0291
// #endif /* WINVER >= 0x0400 */

// #if((_WIN32_WINNT >= 0x0400) || (WINVER >= 0x0500))
WM_MOUSEHOVER                   :: 0x02A1
WM_MOUSELEAVE                   :: 0x02A3
// #endif
// #if(WINVER >= 0x0500)
WM_NCMOUSEHOVER                 :: 0x02A0
WM_NCMOUSELEAVE                 :: 0x02A2
// #endif /* WINVER >= 0x0500 */

// #if(_WIN32_WINNT >= 0x0501)
WM_WTSSESSION_CHANGE            :: 0x02B1

WM_TABLET_FIRST                 :: 0x02c0
WM_TABLET_LAST                  :: 0x02df
// #endif /* _WIN32_WINNT >= 0x0501 */

// #if(WINVER >= 0x0601)
WM_DPICHANGED                   :: 0x02E0
// #endif /* WINVER >= 0x0601 */
// #if(WINVER >= 0x0605)
WM_DPICHANGED_BEFOREPARENT      :: 0x02E2
WM_DPICHANGED_AFTERPARENT       :: 0x02E3
WM_GETDPISCALEDSIZE             :: 0x02E4
// #endif /* WINVER >= 0x0605 */


WM_CUT                          :: 0x0300
WM_COPY                         :: 0x0301
WM_PASTE                        :: 0x0302
WM_CLEAR                        :: 0x0303
WM_UNDO                         :: 0x0304
WM_RENDERFORMAT                 :: 0x0305
WM_RENDERALLFORMATS             :: 0x0306
WM_DESTROYCLIPBOARD             :: 0x0307
WM_DRAWCLIPBOARD                :: 0x0308
WM_PAINTCLIPBOARD               :: 0x0309
WM_VSCROLLCLIPBOARD             :: 0x030A
WM_SIZECLIPBOARD                :: 0x030B
WM_ASKCBFORMATNAME              :: 0x030C
WM_CHANGECBCHAIN                :: 0x030D
WM_HSCROLLCLIPBOARD             :: 0x030E
WM_QUERYNEWPALETTE              :: 0x030F
WM_PALETTEISCHANGING            :: 0x0310
WM_PALETTECHANGED               :: 0x0311
WM_HOTKEY                       :: 0x0312

// #if(WINVER >= 0x0400)
WM_PRINT                        :: 0x0317
WM_PRINTCLIENT                  :: 0x0318
// #endif /* WINVER >= 0x0400 */

// #if(_WIN32_WINNT >= 0x0500)
WM_APPCOMMAND                   :: 0x0319
// #endif /* _WIN32_WINNT >= 0x0500 */

// #if(_WIN32_WINNT >= 0x0501)
WM_THEMECHANGED                 :: 0x031A
// #endif /* _WIN32_WINNT >= 0x0501 */


// #if(_WIN32_WINNT >= 0x0501)
WM_CLIPBOARDUPDATE              :: 0x031D
// #endif /* _WIN32_WINNT >= 0x0501 */

// #if(_WIN32_WINNT >= 0x0600)
WM_DWMCOMPOSITIONCHANGED        :: 0x031E
WM_DWMNCRENDERINGCHANGED        :: 0x031F
WM_DWMCOLORIZATIONCOLORCHANGED  :: 0x0320
WM_DWMWINDOWMAXIMIZEDCHANGE     :: 0x0321
// #endif /* _WIN32_WINNT >= 0x0600 */

// #if(_WIN32_WINNT >= 0x0601)
WM_DWMSENDICONICTHUMBNAIL           :: 0x0323
WM_DWMSENDICONICLIVEPREVIEWBITMAP   :: 0x0326
// #endif /* _WIN32_WINNT >= 0x0601 */


// #if(WINVER >= 0x0600)
WM_GETTITLEBARINFOEX            :: 0x033F
// #endif /* WINVER >= 0x0600 */

// #if(WINVER >= 0x0400)
// #endif /* WINVER >= 0x0400 */


// #if(WINVER >= 0x0400)
WM_HANDHELDFIRST                :: 0x0358
WM_HANDHELDLAST                 :: 0x035F

WM_AFXFIRST                     :: 0x0360
WM_AFXLAST                      :: 0x037F
// #endif /* WINVER >= 0x0400 */

WM_PENWINFIRST                  :: 0x0380
WM_PENWINLAST                   :: 0x038F


// #if(WINVER >= 0x0400)
WM_APP                          :: 0x8000
// #endif /* WINVER >= 0x0400 */


/*
 * NOTE: All Message Numbers below 0x0400 are RESERVED.
 *
 * Private Window Messages Start Here:
 */
WM_USER                         :: 0x0400


RIM_TYPEMOUSE    :: 0 // Raw input comes from the mouse.
RIM_TYPEKEYBOARD :: 1 // Raw input comes from the keyboard.
RIM_TYPEHID      :: 2 // Raw input comes from some device that is not a keyboard or a mouse.


RID_HEADER :: 0x10000005 // Get the header information from the RAWINPUT structure.
RID_INPUT  :: 0x10000003 // Get the raw data from the RAWINPUT structure.

RI_MOUSE_LEFT_BUTTON_DOWN   :: 0x0001 // Left button changed to down.
RI_MOUSE_LEFT_BUTTON_UP     :: 0x0002 // Left button changed to up.
RI_MOUSE_RIGHT_BUTTON_DOWN  :: 0x0004 // Right button changed to down.
RI_MOUSE_RIGHT_BUTTON_UP    :: 0x0008 // Right button changed to up.
RI_MOUSE_MIDDLE_BUTTON_DOWN :: 0x0010 // Middle button changed to down.
RI_MOUSE_MIDDLE_BUTTON_UP   :: 0x0020 // Middle button changed to up.
RI_MOUSE_BUTTON_4_DOWN      :: 0x0040 // XBUTTON1 changed to down.
RI_MOUSE_BUTTON_4_UP        :: 0x0080 // XBUTTON1 changed to up.
RI_MOUSE_BUTTON_5_DOWN      :: 0x0100 // XBUTTON2 changed to down.
RI_MOUSE_BUTTON_5_UP        :: 0x0200 // XBUTTON2 changed to up.
RI_MOUSE_WHEEL              :: 0x0400 // Raw input comes from a mouse wheel. The wheel delta is stored in usButtonData. A positive value indicates that the wheel was rotated forward, away from the user a negative value indicates that the wheel was rotated backward, toward the user. For further information see the following Remarks section.
RI_MOUSE_HWHEEL             :: 0x0800 // Raw input comes from a horizontal mouse wheel. The wheel delta is stored in usButtonData. A positive value indicates that the wheel was rotated to the right a negative value indicates that the wheel was rotated to the left. For further information see the following Remarks section. Windows XP/2000: This value is not supported.

RI_MOUSE_BUTTON_1_DOWN :: RI_MOUSE_LEFT_BUTTON_DOWN
RI_MOUSE_BUTTON_1_UP   :: RI_MOUSE_LEFT_BUTTON_UP
RI_MOUSE_BUTTON_2_DOWN :: RI_MOUSE_RIGHT_BUTTON_DOWN
RI_MOUSE_BUTTON_2_UP   :: RI_MOUSE_RIGHT_BUTTON_UP
RI_MOUSE_BUTTON_3_DOWN :: RI_MOUSE_MIDDLE_BUTTON_DOWN
RI_MOUSE_BUTTON_3_UP   :: RI_MOUSE_MIDDLE_BUTTON_UP


IDC_APPSTARTING :: 32650 as *U8
IDC_ARROW       :: 32512 as *U8
IDC_CROSS       :: 32515 as *U8
IDC_HAND        :: 32649 as *U8
IDC_HELP        :: 32651 as *U8
IDC_IBEAM       :: 32513 as *U8
IDC_ICON        :: 32641 as *U8
IDC_NO          :: 32648 as *U8
IDC_SIZE        :: 32640 as *U8
IDC_SIZEALL     :: 32646 as *U8
IDC_SIZENESW    :: 32643 as *U8
IDC_SIZENS      :: 32645 as *U8
IDC_SIZENWSE    :: 32642 as *U8
IDC_SIZEWE      :: 32644 as *U8
IDC_UPARROW     :: 32516 as *U8
IDC_WAIT        :: 32514 as *U8


/*
 * Virtual Keys, Standard Set
 */
 VK_LBUTTON        :: 0x01
 VK_RBUTTON        :: 0x02
 VK_CANCEL         :: 0x03
 VK_MBUTTON        :: 0x04    /* NOT contiguous with L & RBUTTON */
 VK_XBUTTON1       :: 0x05    /* NOT contiguous with L & RBUTTON */
 VK_XBUTTON2       :: 0x06    /* NOT contiguous with L & RBUTTON */
 VK_BACK           :: 0x08
 VK_TAB            :: 0x09
 VK_CLEAR          :: 0x0C
 VK_RETURN         :: 0x0D
 VK_SHIFT          :: 0x10
 VK_CONTROL        :: 0x11
 VK_MENU           :: 0x12
 VK_PAUSE          :: 0x13
 VK_CAPITAL        :: 0x14
 VK_KANA           :: 0x15
 VK_HANGEUL        :: 0x15  /* old name - should be here for compatibility */
 VK_HANGUL         :: 0x15
 VK_JUNJA          :: 0x17
 VK_FINAL          :: 0x18
 VK_HANJA          :: 0x19
 VK_KANJI          :: 0x19
 VK_ESCAPE         :: 0x1B
 VK_CONVERT        :: 0x1C
 VK_NONCONVERT     :: 0x1D
 VK_ACCEPT         :: 0x1E
 VK_MODECHANGE     :: 0x1F
 VK_SPACE          :: 0x20
 VK_PRIOR          :: 0x21
 VK_NEXT           :: 0x22
 VK_END            :: 0x23
 VK_HOME           :: 0x24
 VK_LEFT           :: 0x25
 VK_UP             :: 0x26
 VK_RIGHT          :: 0x27
 VK_DOWN           :: 0x28
 VK_SELECT         :: 0x29
 VK_PRINT          :: 0x2A
 VK_EXECUTE        :: 0x2B
 VK_SNAPSHOT       :: 0x2C
 VK_INSERT         :: 0x2D
 VK_DELETE         :: 0x2E
 VK_HELP           :: 0x2F
 VK_LWIN           :: 0x5B
 VK_RWIN           :: 0x5C
 VK_APPS           :: 0x5D
 VK_SLEEP          :: 0x5F
 VK_NUMPAD0        :: 0x60
 VK_NUMPAD1        :: 0x61
 VK_NUMPAD2        :: 0x62
 VK_NUMPAD3        :: 0x63
 VK_NUMPAD4        :: 0x64
 VK_NUMPAD5        :: 0x65
 VK_NUMPAD6        :: 0x66
 VK_NUMPAD7        :: 0x67
 VK_NUMPAD8        :: 0x68
 VK_NUMPAD9        :: 0x69
 VK_MULTIPLY       :: 0x6A
 VK_ADD            :: 0x6B
 VK_SEPARATOR      :: 0x6C
 VK_SUBTRACT       :: 0x6D
 VK_DECIMAL        :: 0x6E
 VK_DIVIDE         :: 0x6F
 VK_F1             :: 0x70
 VK_F2             :: 0x71
 VK_F3             :: 0x72
 VK_F4             :: 0x73
 VK_F5             :: 0x74
 VK_F6             :: 0x75
 VK_F7             :: 0x76
 VK_F8             :: 0x77
 VK_F9             :: 0x78
 VK_F10            :: 0x79
 VK_F11            :: 0x7A
 VK_F12            :: 0x7B
 VK_F13            :: 0x7C
 VK_F14            :: 0x7D
 VK_F15            :: 0x7E
 VK_F16            :: 0x7F
 VK_F17            :: 0x80
 VK_F18            :: 0x81
 VK_F19            :: 0x82
 VK_F20            :: 0x83
 VK_F21            :: 0x84
 VK_F22            :: 0x85
 VK_F23            :: 0x86
 VK_F24            :: 0x87
 VK_NAVIGATION_VIEW     :: 0x88 // reserved
 VK_NAVIGATION_MENU     :: 0x89 // reserved
 VK_NAVIGATION_UP       :: 0x8A // reserved
 VK_NAVIGATION_DOWN     :: 0x8B // reserved
 VK_NAVIGATION_LEFT     :: 0x8C // reserved
 VK_NAVIGATION_RIGHT    :: 0x8D // reserved
 VK_NAVIGATION_ACCEPT   :: 0x8E // reserved
 VK_NAVIGATION_CANCEL   :: 0x8F // reserved
 VK_NUMLOCK        :: 0x90
 VK_SCROLL         :: 0x91
 VK_OEM_NEC_EQUAL  :: 0x92   // '=' key on numpad
 VK_OEM_FJ_JISHO   :: 0x92   // 'Dictionary' key
 VK_OEM_FJ_MASSHOU :: 0x93   // 'Unregister word' key
 VK_OEM_FJ_TOUROKU :: 0x94   // 'Register word' key
 VK_OEM_FJ_LOYA    :: 0x95   // 'Left OYAYUBI' key
 VK_OEM_FJ_ROYA    :: 0x96   // 'Right OYAYUBI' key
 VK_LSHIFT         :: 0xA0
 VK_RSHIFT         :: 0xA1
 VK_LCONTROL       :: 0xA2
 VK_RCONTROL       :: 0xA3
 VK_LMENU          :: 0xA4
 VK_RMENU          :: 0xA5
 VK_BROWSER_BACK        :: 0xA6
 VK_BROWSER_FORWARD     :: 0xA7
 VK_BROWSER_REFRESH     :: 0xA8
 VK_BROWSER_STOP        :: 0xA9
 VK_BROWSER_SEARCH      :: 0xAA
 VK_BROWSER_FAVORITES   :: 0xAB
 VK_BROWSER_HOME        :: 0xAC
 VK_VOLUME_MUTE         :: 0xAD
 VK_VOLUME_DOWN         :: 0xAE
 VK_VOLUME_UP           :: 0xAF
 VK_MEDIA_NEXT_TRACK    :: 0xB0
 VK_MEDIA_PREV_TRACK    :: 0xB1
 VK_MEDIA_STOP          :: 0xB2
 VK_MEDIA_PLAY_PAUSE    :: 0xB3
 VK_LAUNCH_MAIL         :: 0xB4
 VK_LAUNCH_MEDIA_SELECT :: 0xB5
 VK_LAUNCH_APP1         :: 0xB6
 VK_LAUNCH_APP2         :: 0xB7
 VK_OEM_1          :: 0xBA   // ';:' for US
 VK_OEM_PLUS       :: 0xBB   // '+' any country
 VK_OEM_COMMA      :: 0xBC   // ',' any country
 VK_OEM_MINUS      :: 0xBD   // '-' any country
 VK_OEM_PERIOD     :: 0xBE   // '.' any country
 VK_OEM_2          :: 0xBF   // '/?' for US
 VK_OEM_3          :: 0xC0   // '`~' for US
 VK_GAMEPAD_A                         :: 0xC3 // reserved
 VK_GAMEPAD_B                         :: 0xC4 // reserved
 VK_GAMEPAD_X                         :: 0xC5 // reserved
 VK_GAMEPAD_Y                         :: 0xC6 // reserved
 VK_GAMEPAD_RIGHT_SHOULDER            :: 0xC7 // reserved
 VK_GAMEPAD_LEFT_SHOULDER             :: 0xC8 // reserved
 VK_GAMEPAD_LEFT_TRIGGER              :: 0xC9 // reserved
 VK_GAMEPAD_RIGHT_TRIGGER             :: 0xCA // reserved
 VK_GAMEPAD_DPAD_UP                   :: 0xCB // reserved
 VK_GAMEPAD_DPAD_DOWN                 :: 0xCC // reserved
 VK_GAMEPAD_DPAD_LEFT                 :: 0xCD // reserved
 VK_GAMEPAD_DPAD_RIGHT                :: 0xCE // reserved
 VK_GAMEPAD_MENU                      :: 0xCF // reserved
 VK_GAMEPAD_VIEW                      :: 0xD0 // reserved
 VK_GAMEPAD_LEFT_THUMBSTICK_BUTTON    :: 0xD1 // reserved
 VK_GAMEPAD_RIGHT_THUMBSTICK_BUTTON   :: 0xD2 // reserved
 VK_GAMEPAD_LEFT_THUMBSTICK_UP        :: 0xD3 // reserved
 VK_GAMEPAD_LEFT_THUMBSTICK_DOWN      :: 0xD4 // reserved
 VK_GAMEPAD_LEFT_THUMBSTICK_RIGHT     :: 0xD5 // reserved
 VK_GAMEPAD_LEFT_THUMBSTICK_LEFT      :: 0xD6 // reserved
 VK_GAMEPAD_RIGHT_THUMBSTICK_UP       :: 0xD7 // reserved
 VK_GAMEPAD_RIGHT_THUMBSTICK_DOWN     :: 0xD8 // reserved
 VK_GAMEPAD_RIGHT_THUMBSTICK_RIGHT    :: 0xD9 // reserved
 VK_GAMEPAD_RIGHT_THUMBSTICK_LEFT     :: 0xDA // reserved
 VK_OEM_4          :: 0xDB  //  '[{' for US
 VK_OEM_5          :: 0xDC  //  '\|' for US
 VK_OEM_6          :: 0xDD  //  ']}' for US
 VK_OEM_7          :: 0xDE  //  ''"' for US
 VK_OEM_8          :: 0xDF
 VK_OEM_AX         :: 0xE1  //  'AX' key on Japanese AX kbd
 VK_OEM_102        :: 0xE2  //  "<>" or "\|" on RT 102-key kbd.
 VK_ICO_HELP       :: 0xE3  //  Help key on ICO
 VK_ICO_00         :: 0xE4  //  00 key on ICO
 VK_PROCESSKEY     :: 0xE5
 VK_ICO_CLEAR      :: 0xE6
 VK_PACKET         :: 0xE7
 VK_OEM_RESET      :: 0xE9
 VK_OEM_JUMP       :: 0xEA
 VK_OEM_PA1        :: 0xEB
 VK_OEM_PA2        :: 0xEC
 VK_OEM_PA3        :: 0xED
 VK_OEM_WSCTRL     :: 0xEE
 VK_OEM_CUSEL      :: 0xEF
 VK_OEM_ATTN       :: 0xF0
 VK_OEM_FINISH     :: 0xF1
 VK_OEM_COPY       :: 0xF2
 VK_OEM_AUTO       :: 0xF3
 VK_OEM_ENLW       :: 0xF4
 VK_OEM_BACKTAB    :: 0xF5
 VK_ATTN           :: 0xF6
 VK_CRSEL          :: 0xF7
 VK_EXSEL          :: 0xF8
 VK_EREOF          :: 0xF9
 VK_PLAY           :: 0xFA
 VK_ZOOM           :: 0xFB
 VK_NONAME         :: 0xFC
 VK_PA1            :: 0xFD
 VK_OEM_CLEAR      :: 0xFE


#extern_library "kernel32"

GetStdHandle :: (nStdHandle: DWORD) HANDLE

WriteConsoleA :: (hConsoleOutput: HANDLE, lpBuffer: LPCVOID, nNumberOfCharsToWrite: DWORD, lpNumberOfCharsWritten: LPDWORD, lpReserved: LPVOID) BOOL

SetConsoleTextAttribute :: (hConsoleOutput: HANDLE, attribute: DWORD)

CreateFileA :: (
	lpFileName: LPCSTR,
	dwDesiredAccess: DWORD,
	dwShareMode: DWORD,
	lpSecurityAttributes: *SECURITY_ATTRIBUTES,
	dwCreationDisposition: DWORD,
	dwFlagsAndAttributes: DWORD,
	hTemplateFile: HANDLE
) HANDLE

WriteFile :: (
	hFile:                  HANDLE,
	lpBuffer:               LPCVOID,
	nNumberOfBytesToWrite:  DWORD,
	lpNumberOfBytesWritten: LPDWORD,
	lpOverlapped:           *OVERLAPPED
) BOOL

GetLastError :: () DWORD

VirtualAlloc :: (
	lpAddress        : LPVOID,
	dwSize           : SIZE_T,
	flAllocationType : DWORD,
	flProtect        : DWORD
) LPVOID

VirtualFree :: (
	lpAddress: LPVOID,
	dwSize: SIZE_T,
	dwFreeType: DWORD
) BOOL

GetModuleHandleA :: (
	lpModuleName : LPCSTR
) HMODULE

ExitProcess :: (ret: UINT)

#extern_library "user32"

RegisterClassExA :: (
	unnamedParam1 : *WNDCLASSEXA
) ATOM

DefWindowProcA :: (
	hWnd   : HWND,
	Msg    : UINT,
	wParam : WPARAM,
	lParam : LPARAM
) LRESULT

CreateWindowExA :: (
	dwExStyle    : DWORD,
	lpClassName  : LPCSTR,
	lpWindowName : LPCSTR,
	dwStyle      : DWORD,
	X            : INT,
	Y            : INT,
	nWidth       : INT,
	nHeight      : INT,
	hWndParent   : HWND,
	hMenu        : HMENU,
	hInstance    : HINSTANCE,
	lpParam      : LPVOID
) HWND

PeekMessageA :: (
	lpMsg         : LPMSG,
	hWnd          : HWND,
	wMsgFilterMin : UINT,
	wMsgFilterMax : UINT,
	wRemoveMsg    : UINT
) BOOL

TranslateMessage :: (
	lpMsg : *MSG
) BOOL

DispatchMessageA :: (
	lpMsg : *MSG
) LRESULT

PostQuitMessage :: (
  nExitCode : INT
)

LoadCursorA :: (
  hInstance    : HINSTANCE,
  lpCursorName : LPCSTR
) HCURSOR

GetDC :: (
  hWnd : HWND
) HDC

RegisterRawInputDevices :: (
    pRawInputDevices: PCRAWINPUTDEVICE,
    uiNumDevices: UINT,
    cbSize: UINT
) BOOL

SetCapture :: (hWnd: HWND) HWND

ReleaseCapture :: () BOOL

GetRawInputData :: (
  hRawInput:    HRAWINPUT,
  uiCommand:    UINT,
  pData:	    LPVOID,
  pcbSize:	    PUINT,
  cbSizeHeader: UINT
) UINT

GetCursorPos :: (p: *POINT)

ScreenToClient :: (window: HWND, p: *POINT)

SetFilePointerEx :: (
	hFile: HANDLE,
	liDistanceToMove: LARGE_INTEGER,
	lpNewFilePointer: PLARGE_INTEGER,
	dwMoveMethod: DWORD
) BOOL

ReadFile :: (
	hFile: HANDLE,
	lpBuffer: LPVOID,
	nNumberOfBytesToRead: DWORD,
	lpNumberOfBytesRead: LPDWORD,
	lpOverlapped: LPOVERLAPPED
) BOOL