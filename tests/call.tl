import "windows.tl"
main :: fn (): uint {
    f := WriteConsoleA;
    handle := GetStdHandle(STD_OUTPUT_HANDLE);
    data := "Hello\n".data;
    count := "Hello\n".count as u32;
    
    WriteConsoleA(handle, data, count, null, null);
    (WriteConsoleA)(handle, data, count, null, null);
    f(handle, data, count, null, null);
    (f)(handle, data, count, null, null);
    (*&f)(handle, data, count, null, null);
}
