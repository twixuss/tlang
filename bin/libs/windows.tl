HANDLE :: *void;

STD_INPUT_HANDLE  :: -10; // The standard input device. Initially, this is the console input buffer, CONIN$.
STD_OUTPUT_HANDLE :: -11; // The standard output device. Initially, this is the active console screen buffer, CONOUT$.
STD_ERROR_HANDLE  :: -12; // The standard error device. Initially, this is the active console screen buffer, CONOUT$.

extern "C" "kernel32.lib" {

/*
SECURITY_ATTRIBUTES :: struct {
  DWORD  nLength;
  LPVOID lpSecurityDescriptor;
  BOOL   bInheritHandle;
}

OVERLAPPED :: struct {
	ULONG_PTR Internal;
	ULONG_PTR InternalHigh;
	union {
		struct {
			DWORD Offset;
			DWORD OffsetHigh;
		} DUMMYSTRUCTNAME;
		PVOID Pointer;
	} DUMMYUNIONNAME;
	HANDLE    hEvent;
}
*/

SECURITY_ATTRIBUTES :: void;
OVERLAPPED :: void;

GetStdHandle :: fn (nStdHandle: u32) HANDLE;

WriteConsoleA :: fn (hConsoleOutput: HANDLE, lpBuffer: *void, nNumberOfCharsToWrite: u32, lpNumberOfCharsWritten: *u32, lpReserved: *void) bool;

GENERIC_READ    :u32: 0x80000000;
GENERIC_WRITE   :u32: 0x40000000;
GENERIC_EXECUTE :u32: 0x20000000;
GENERIC_ALL     :u32: 0x10000000;

FILE_SHARE_DELETE :u32: 0x00000004;
FILE_SHARE_READ   :u32: 0x00000001;
FILE_SHARE_WRITE  :u32: 0x00000002;

CREATE_ALWAYS     :u32: 2;
CREATE_NEW        :u32: 1;
OPEN_ALWAYS       :u32: 4;
OPEN_EXISTING     :u32: 3;
TRUNCATE_EXISTING :u32: 5;

FILE_ATTRIBUTE_READONLY      :u32: 0x00000001;
FILE_ATTRIBUTE_HIDDEN        :u32: 0x00000002;
FILE_ATTRIBUTE_SYSTEM        :u32: 0x00000004;
FILE_ATTRIBUTE_ARCHIVE       :u32: 0x00000020;
FILE_ATTRIBUTE_NORMAL        :u32: 0x00000080;
FILE_ATTRIBUTE_TEMPORARY     :u32: 0x00000100;
FILE_ATTRIBUTE_OFFLINE       :u32: 0x00001000;
FILE_ATTRIBUTE_ENCRYPTED     :u32: 0x00004000;
FILE_FLAG_BACKUP_SEMANTICS   :u32: 0x02000000;
FILE_FLAG_DELETE_ON_CLOSE    :u32: 0x04000000;
FILE_FLAG_NO_BUFFERING       :u32: 0x20000000;
FILE_FLAG_OPEN_NO_RECALL     :u32: 0x00100000;
FILE_FLAG_OPEN_REPARSE_POINT :u32: 0x00200000;
FILE_FLAG_OVERLAPPED         :u32: 0x40000000;
FILE_FLAG_POSIX_SEMANTICS    :u32: 0x01000000;
FILE_FLAG_RANDOM_ACCESS      :u32: 0x10000000;
FILE_FLAG_SESSION_AWARE      :u32: 0x00800000;
FILE_FLAG_SEQUENTIAL_SCAN    :u32: 0x08000000;
FILE_FLAG_WRITE_THROUGH      :u32: 0x80000000;

CreateFileA :: fn (
	lpFileName: *void,
	dwDesiredAccess: u32,
	dwShareMode: u32,
	lpSecurityAttributes: *SECURITY_ATTRIBUTES,
	dwCreationDisposition: u32,
	dwFlagsAndAttributes: u32,
	hTemplateFile: HANDLE
) HANDLE;

WriteFile :: fn (
	hFile:                  HANDLE,
	lpBuffer:               *void,
	nNumberOfBytesToWrite:  u32,
	lpNumberOfBytesWritten: *u32,
	lpOverlapped:           *OVERLAPPED
) bool;

GetLastError :: fn () u32;

}
