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
    
    #assert !#compiles { a: bool = 1; };
    #assert  #compiles { a: bool = true; };
    
    #assert  #compiles { a: u8     = U8; };
    #assert  #compiles { a: u16    = U8; };
    #assert  #compiles { a: u32    = U8; };
    #assert  #compiles { a: u64    = U8; };
    #assert !#compiles { a: s8     = U8; };
    #assert  #compiles { a: s16    = U8; };
    #assert  #compiles { a: s32    = U8; };
    #assert  #compiles { a: s64    = U8; };
    #assert !#compiles { a: bool   = U8; };
    #assert !#compiles { a: string = U8; };
    #assert !#compiles { a: *void  = U8; };
    
    #assert !#compiles { a: u8  = U16; };
    #assert  #compiles { a: u16 = U16; };
    #assert  #compiles { a: u32 = U16; };
    #assert  #compiles { a: u64 = U16; };
    #assert !#compiles { a: s8  = U16; };
    #assert !#compiles { a: s16 = U16; };
    #assert  #compiles { a: s32 = U16; };
    #assert  #compiles { a: s64 = U16; };
    
    #assert !#compiles { a: u8  = U32; };
    #assert !#compiles { a: u16 = U32; };
    #assert  #compiles { a: u32 = U32; };
    #assert  #compiles { a: u64 = U32; };
    #assert !#compiles { a: s8  = U32; };
    #assert !#compiles { a: s16 = U32; };
    #assert !#compiles { a: s32 = U32; };
    #assert  #compiles { a: s64 = U32; };
    
    #assert !#compiles { a: u8  = U64; };
    #assert !#compiles { a: u16 = U64; };
    #assert !#compiles { a: u32 = U64; };
    #assert  #compiles { a: u64 = U64; };
    #assert !#compiles { a: s8  = U64; };
    #assert !#compiles { a: s16 = U64; };
    #assert !#compiles { a: s32 = U64; };
    #assert !#compiles { a: s64 = U64; };
    
    #assert !#compiles { a: u8  = S8; };
    #assert !#compiles { a: u16 = S8; };
    #assert !#compiles { a: u32 = S8; };
    #assert !#compiles { a: u64 = S8; };
    #assert  #compiles { a: s8  = S8; };
    #assert  #compiles { a: s16 = S8; };
    #assert  #compiles { a: s32 = S8; };
    #assert  #compiles { a: s64 = S8; };
    
    #assert !#compiles { a: u8  = S16; };
    #assert !#compiles { a: u16 = S16; };
    #assert !#compiles { a: u32 = S16; };
    #assert !#compiles { a: u64 = S16; };
    #assert !#compiles { a: s8  = S16; };
    #assert  #compiles { a: s16 = S16; };
    #assert  #compiles { a: s32 = S16; };
    #assert  #compiles { a: s64 = S16; };
    
    #assert !#compiles { a: u8  = S32; };
    #assert !#compiles { a: u16 = S32; };
    #assert !#compiles { a: u32 = S32; };
    #assert !#compiles { a: u64 = S32; };
    #assert !#compiles { a: s8  = S32; };
    #assert !#compiles { a: s16 = S32; };
    #assert  #compiles { a: s32 = S32; };
    #assert  #compiles { a: s64 = S32; };
    
    #assert !#compiles { a: u8  = S64; };
    #assert !#compiles { a: u16 = S64; };
    #assert !#compiles { a: u32 = S64; };
    #assert !#compiles { a: u64 = S64; };
    #assert !#compiles { a: s8  = S64; };
    #assert !#compiles { a: s16 = S64; };
    #assert !#compiles { a: s32 = S64; };
    #assert  #compiles { a: s64 = S64; };
}