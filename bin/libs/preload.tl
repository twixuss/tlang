sint :: s64;
uint :: u64;

int :: s64;

smm :: s64;
umm :: u64;

/*

allocator :: struct {
    func:  #type fn (size: umm, state: *void) -> *void;
    state: *void;
}

default_allocator: allocator;

__init_allocator :: fn () {
    default_allocator.func = fn (size: umm, state: *void) -> *void {
        return null;
    };
}

allocate :: fn (size: umm) -> *void {
    return default_allocator.func(size, null);
}

*/
