import "windows.tl"

print :: fn (str : string) {
    WriteConsoleA(GetStdHandle(STD_OUTPUT_HANDLE), str.data, str.count, null, null);
}
