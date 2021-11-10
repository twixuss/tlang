#define TL_IMPL
#include <tl/main.h>
#include <tl/file.h>
#include <tl/process.h>

using namespace tl;

s32 tl_main(Span<Span<utf8>> arguments) {
	for (auto item : get_items_in_directory(u"../tests/"s)) {
		if (item.kind == FileItem_file && ends_with(item.name, u".tl"s)) {
			print("%\n", item.name);
			auto process = start_process(format(u"tlang.exe ../tests/% --output nasm"s, item.name));
			assert(is_valid(process));

			wait(process);
			if (get_exit_code(process) != 0) {
				print("Test failed on file '%'\n", item.name);
				return 1;
			}
		}
	}

	return 0;
}

