import "tlang/windows.tl"
main :: () {
    foo := WriteFile
    bar :: (a: HANDLE, b: *U8, c: U32, d: *DWORD, e: *OVERLAPPED) => WriteFile(a, b, c, d, e)
    foobar := bar

    handle := GetStdHandle(STD_OUTPUT_HANDLE)
    data := "Hello\n".data
    count := "Hello\n".count as U32

    WriteFile(handle, data, count, null, null)
    (WriteFile)(handle, data, count, null, null)
    foo(handle, data, count, null, null)
    (foo)(handle, data, count, null, null)
    (*&foo)(handle, data, count, null, null)
    bar(handle, data, count, null, null)
    (bar)(handle, data, count, null, null)
    (*&bar)(handle, data, count, null, null)
    foobar(handle, data, count, null, null)
    (foobar)(handle, data, count, null, null)
    (*&foobar)(handle, data, count, null, null)
    0
}
