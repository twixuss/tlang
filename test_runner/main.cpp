#define TL_IMPL
#include <tl/main.h>
#include <tl/file.h>
#include <tl/process.h>

using namespace tl;

s32 tl_main(Span<Span<utf8>> arguments) {
	for (auto item : get_items_in_directory(u"../tests/"s)) {
		if (item.kind == FileItem_file && ends_with(item.name, u".tl"s)) {
			print("{}\n", item.name);
			auto process = start_process(format(u"tlang.exe ../tests/{} --output nasm"s, item.name));
			assert(is_valid(process));

			u8 buf[256];

			StringBuilder builder;
			while (1) {
				auto bytes_read = process.standard_out->read(array_as_span(buf));
				if (!bytes_read)
					break;
				append(builder, Span((utf8 *)buf, bytes_read));
			}

			wait(process);
			if (get_exit_code(process) != 0) {
				print("Test failed on file '{}'\n", item.name);
				print(to_string(builder));
				return 1;
			}
		}
	}

	return 0;
}

