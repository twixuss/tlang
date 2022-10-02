
#include <Windows.h>

static void *my_malloc(size_t size) {
    return VirtualAlloc(0, size, MEM_COMMIT|MEM_RESERVE, PAGE_READWRITE);
}
static void my_free(void *data) {
    VirtualFree(data, 0, MEM_FREE);
}
static void *my_realloc(void *old_data, size_t old_size, size_t new_size) {
    auto new_data = my_malloc(new_size);
    memcpy(new_data, old_data, old_size);
    my_free(old_data);
    return new_data;
}

#define STB_IMAGE_IMPLEMENTATION
#define STBI_NO_STDIO
#define STBI_NO_LINEAR
#define STBI_NO_HDR

#define STBI_MALLOC my_malloc
#define STBI_FREE my_free
#define STBI_REALLOC_SIZED my_realloc

#include "../dep/stb/stb_image.h"
