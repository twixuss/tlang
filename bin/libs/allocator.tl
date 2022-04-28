import "windows.tl"

allocator :: struct {
    func:  #type fn (size: uint, state: *void): *void;
    state: *void;
}

default_allocator: allocator;

init_allocator :: fn () {
    default_allocator.func = fn (size: uint, state: *void): *void {
        return VirtualAlloc(null, size, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
    };
}

new :: fn (T: %type): *T {
    return @ default_allocator.func(#sizeof T, null);
}
