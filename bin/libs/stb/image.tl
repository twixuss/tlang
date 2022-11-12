#stdcall

#extern_library "msvcrt"
__dummy :: ()

#extern_library "stb"
stbi_load_from_memory :: (buffer: *U8, len: S32, width: *S32, height: *S32, actual_comp: *S32, desired_comp: S32) *U8