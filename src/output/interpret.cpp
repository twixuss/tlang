#define TL_IMPL
#pragma warning(disable: 4702) // unreachable
#include "../interpret.h"

DECLARE_OUTPUT_BUILDER {
	init_allocator();
	init_printer();

	run_bytecode(compiler, bytecode.instructions, compiler.main_lambda, compiler.extern_libraries);
}

DECLARE_TARGET_INFORMATION_GETTER {
	compiler.stack_word_size = 8;
	compiler.register_size = 8;
	compiler.general_purpose_register_count = 16;
}
