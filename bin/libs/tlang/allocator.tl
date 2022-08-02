import "tlang/windows.tl"

allocator :: struct {
    func:  (size: UInt, state: *Void): *Void #type
    state: *Void
}

default_allocator: allocator

init_allocator :: () {
    default_allocator.func = (size: UInt, state: *Void): *Void {
        return VirtualAlloc(null, size, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE)
    }
}

new :: (T: %Type): *T {
    return @ default_allocator.func(#sizeof T, null)
}
