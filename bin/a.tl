HANDLE :: *void;

STD_INPUT_HANDLE  :: -10; // The standard input device. Initially, this is the console input buffer, CONIN$.
STD_OUTPUT_HANDLE :: -11; // The standard output device. Initially, this is the active console screen buffer, CONOUT$.
STD_ERROR_HANDLE  :: -12; // The standard error device. Initially, this is the active console screen buffer, CONOUT$.

/* 
	HALLO
	/*
		This is a comment inside another comment !
	*/
*/

extern "C" "kernel32.lib" {

GetStdHandle :: fn (nStdHandle: u32) HANDLE;
WriteConsoleA :: fn (hConsoleOutput: HANDLE, lpBuffer: *void, nNumberOfCharsToWrite: u32, lpNumberOfCharsWritten: *u32, lpReserved: *void) bool;

}

null : *void : 0;


main :: fn () s32 {
    handle := GetStdHandle(STD_OUTPUT_HANDLE);
    str : string = "Hello World!";
    WriteConsoleA(handle, str.data, str.count, null, null);
    WriteConsoleA(handle, "Hello World!".data, 12, null, null);
    print("Hello World!");

    return x;
}


x :: 42;

print :: fn (str : string) {
    handle := GetStdHandle(STD_OUTPUT_HANDLE);
    WriteConsoleA(handle, str.data, str.count, null, null);
}