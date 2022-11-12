#extern_library "freetype"
#layout_c
#stdcall

FT_Error :: S32

FT_Int :: Int
FT_UInt :: UInt

FT_LibraryRec :: struct {}
FT_Library :: *FT_LibraryRec;

FT_Init_FreeType :: (alibrary: *FT_Library) FT_Error

FT_Error_String :: (error_code: FT_Error) CString