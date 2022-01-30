main :: fn () {
    CONST :: 125;
    
    U8  : u8   = CONST;
    U16 : u16  = CONST;
    U32 : u32  = CONST;
    U64 : u64  = CONST;
    S8  : s8   = CONST;
    S16 : s16  = CONST;
    S32 : s32  = CONST;
    S64 : s64  = CONST;
    BOOL: bool = true;
    STR : string = "HI";
    PTR : *void = null;
    
    #test false { a: bool = 1; }
    #test true  { a: bool = true; }
    
    #test true  { a: u8     = U8; }
    #test true  { a: u16    = U8; }
    #test true  { a: u32    = U8; }
    #test true  { a: u64    = U8; }
    #test false { a: s8     = U8; }
    #test true  { a: s16    = U8; }
    #test true  { a: s32    = U8; }
    #test true  { a: s64    = U8; }
    #test false { a: bool   = U8; }
    #test false { a: string = U8; }
    #test false { a: *void  = U8; }
    
    #test false { a: u8  = U16; }
    #test true  { a: u16 = U16; }
    #test true  { a: u32 = U16; }
    #test true  { a: u64 = U16; }
    #test false { a: s8  = U16; }
    #test false { a: s16 = U16; }
    #test true  { a: s32 = U16; }
    #test true  { a: s64 = U16; }
    
    #test false { a: u8  = U32; }
    #test false { a: u16 = U32; }
    #test true  { a: u32 = U32; }
    #test true  { a: u64 = U32; }
    #test false { a: s8  = U32; }
    #test false { a: s16 = U32; }
    #test false { a: s32 = U32; }
    #test true  { a: s64 = U32; }
    
    #test false { a: u8  = U64; }
    #test false { a: u16 = U64; }
    #test false { a: u32 = U64; }
    #test true  { a: u64 = U64; }
    #test false { a: s8  = U64; }
    #test false { a: s16 = U64; }
    #test false { a: s32 = U64; }
    #test false { a: s64 = U64; }
    
    #test false { a: u8  = S8; }
    #test false { a: u16 = S8; }
    #test false { a: u32 = S8; }
    #test false { a: u64 = S8; }
    #test true  { a: s8  = S8; }
    #test true  { a: s16 = S8; }
    #test true  { a: s32 = S8; }
    #test true  { a: s64 = S8; }
    
    #test false { a: u8  = S16; }
    #test false { a: u16 = S16; }
    #test false { a: u32 = S16; }
    #test false { a: u64 = S16; }
    #test false { a: s8  = S16; }
    #test true  { a: s16 = S16; }
    #test true  { a: s32 = S16; }
    #test true  { a: s64 = S16; }
    
    #test false { a: u8  = S32; }
    #test false { a: u16 = S32; }
    #test false { a: u32 = S32; }
    #test false { a: u64 = S32; }
    #test false { a: s8  = S32; }
    #test false { a: s16 = S32; }
    #test true  { a: s32 = S32; }
    #test true  { a: s64 = S32; }
    
    #test false { a: u8  = S64; }
    #test false { a: u16 = S64; }
    #test false { a: u32 = S64; }
    #test false { a: u64 = S64; }
    #test false { a: s8  = S64; }
    #test false { a: s16 = S64; }
    #test false { a: s32 = S64; }
    #test true  { a: s64 = S64; }
}