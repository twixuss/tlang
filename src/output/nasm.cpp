#include "nasm.h"
#include "ast.h"
#include "extern.h"

void output_nasm(Bytecode &bytecode) {
	timed_function();


	StringBuilder builder;

	append(builder, "bits 64\n");
	if (bytecode.constant_data.count) {
		append(builder, "section .rodata\nconstants: db ");
		for (auto byte : bytecode.constant_data) {
			append_format(builder, "%,", byte);
		}
		append(builder, '\n');
	}
	if (bytecode.data.count) {
		append(builder, "section .data\ndata: db ");
		for (auto byte : bytecode.data) {
			append_format(builder, "%,", byte);
		}
		append(builder, '\n');
	}
	if (bytecode.zero_data.count) {
		append(builder, "section .bss\nzeros: db ");
		for (auto byte : bytecode.zero_data) {
			append_format(builder, "%,", byte);
		}
		append(builder, '\n');
	}
	append_format(builder, "section .text\nglobal main\nmain:jmp .%\n", FormatInt{.value=main_lambda->location_in_bytecode, .radix=62});

	s64 idx = 0;
	for (auto i : bytecode.instructions) {
		auto in = FormatInt{.value=idx, .radix=62};
		switch (i.kind) {
			using enum InstructionKind;
			case move_reg_to_reg:		          append_format(builder, ".%: mov         %, %        \n", in, i.move_reg_to_reg     .dst_reg , i.move_reg_to_reg     .src_reg  ); break;
			case move_constant_to_reg:	          append_format(builder, ".%: mov         %, %        \n", in, i.move_constant_to_reg.reg     , i.move_constant_to_reg.constant ); break;
			case move_mem_to_reg:		          append_format(builder, ".%: mov         %, qword [%]\n", in, i.move_mem_to_reg     .dst_reg , i.move_mem_to_reg     .src_reg  ); break;
			case move_reg_to_mem:		          append_format(builder, ".%: mov         qword [%], %\n", in, i.move_reg_to_mem     .dst_reg , i.move_reg_to_mem     .src_reg  ); break;
			case push_reg:				          append_format(builder, ".%: push        %           \n", in, i.push_reg            .reg                                       ); break;
			case push_constant:			          append_format(builder, ".%: push        %           \n", in, i.push_constant       .constant                                  ); break;
			case push_mem:				          append_format(builder, ".%: push        qword [%]   \n", in, i.push_mem            .reg                                       ); break;
			case push_constant_data_address:      append_format(builder, ".%: mov rax, constants + %\npush rax\n", in, i.push_constant_data_address.address                                ); break;
			case push_data_address:               append_format(builder, ".%: mov rax, data + %\npush rax\n", in, i.push_data_address.address                                ); break;
			case push_uninitialized_data_address: append_format(builder, ".%: mov rax, zeros + %\npush rax\n", in, i.push_uninitialized_data_address.address                                ); break;
			case pop_reg:				          append_format(builder, ".%: pop         %           \n", in, i.pop_reg             .reg                                       ); break;
			case ret:					          append_format(builder, ".%: ret                     \n", in                                                                   ); break;
			case add_constant_to_reg:	          append_format(builder, ".%: add         %, %        \n", in, i.add_constant_to_reg.reg      , i.add_constant_to_reg.constant  ); break;
			case add_constant_to_mem:	          append_format(builder, ".%: add         qword [%], %\n", in, i.add_constant_to_mem.reg      , i.add_constant_to_mem.constant  ); break;
			case add_reg_to_mem:		          append_format(builder, ".%: add         qword [%], %\n", in, i.add_reg_to_mem     .dst_reg  , i.add_reg_to_mem     .src_reg   ); break;
			case add_reg_to_reg:		          append_format(builder, ".%: add         %, %        \n", in, i.add_reg_to_reg     .dst_reg  , i.add_reg_to_reg     .src_reg   ); break;
			case sub_constant_to_reg:	          append_format(builder, ".%: sub         %, %        \n", in, i.sub_constant_to_reg.reg      , i.sub_constant_to_reg.constant  ); break;
			case sub_reg_to_reg:		          append_format(builder, ".%: sub         %, %        \n", in, i.sub_reg_to_reg     .dst_reg  , i.sub_reg_to_reg     .src_reg   ); break;
			case sub_reg_to_mem:		          append_format(builder, ".%: sub         qword [%], %\n", in, i.sub_reg_to_mem     .dst_reg  , i.sub_reg_to_mem     .src_reg   ); break;
			case mul_reg_to_mem:		          append_format(builder, ".%: mul         qword [%], %\n", in, i.mul_reg_to_mem     .dst_reg  , i.mul_reg_to_mem     .src_reg   ); break;
			case div_reg_to_mem:		          append_format(builder, ".%: div         qword [%], %\n", in, i.div_reg_to_mem     .dst_reg  , i.div_reg_to_mem     .src_reg   ); break;
			case mod_reg_to_mem:		          append_format(builder, ".%: mod         qword [%], %\n", in, i.mod_reg_to_mem     .dst_reg  , i.mod_reg_to_mem     .src_reg   ); break;
			case or_reg_to_mem:			          append_format(builder, ".%:  or         qword [%], %\n", in, i.or_reg_to_mem      .dst_reg  , i.or_reg_to_mem      .src_reg   ); break;
			case and_constant_to_reg:	          append_format(builder, ".%: and         %, %        \n", in, i.and_constant_to_reg.reg      , i.and_constant_to_reg.constant  ); break;
			case and_reg_to_mem:		          append_format(builder, ".%: and         qword [%], %\n", in, i.and_reg_to_mem     .dst_reg  , i.and_reg_to_mem     .src_reg   ); break;
			case xor_reg_to_reg:		          append_format(builder, ".%: xor         %, %        \n", in, i.xor_reg_to_reg     .dst_reg  , i.xor_reg_to_reg     .src_reg   ); break;
			case xor_reg_to_mem:		          append_format(builder, ".%: xor         qword [%], %\n", in, i.xor_reg_to_mem     .dst_reg  , i.xor_reg_to_mem     .src_reg   ); break;
			case cmp_rax_rbx:			          append_format(builder, ".%: cmp_rax_rbx %, %        \n", in, i.cmp_rax_rbx        .dst_reg  , i.cmp_rax_rbx        .comparison); break;
			case call_constant:			          append_format(builder, ".%: call        %           \n", in, i.call_constant      .constant                                   ); break;
			case call_string:			          append_format(builder, ".%: call        %           \n", in, i.call_string        .string                                     ); break;
			case jmp:					          append_format(builder, ".%: jmp         %           \n", in, i.jmp                .offset                                     ); break;
			case jz:					          append_format(builder, ".%: jz          %, %        \n", in, i.jz                 .reg      , i.jz                 .offset    ); break;
			default:invalid_code_path();
		}
		++idx;
	}


	auto output_path = to_pathchars(format(u8"%.asm", source_path_without_extension));

	{
		auto file = open_file(output_path, {.write = true});
		defer { close(file); };

		write(file, as_bytes(to_string(builder)));
	}


	StringBuilder bat_builder;
	append_format(bat_builder, u8R"(
@echo off
call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvarsall.bat" x64
%\nasm -f win64 -g "%.asm" -o "%.obj"
)", executable_directory, source_path_without_extension, source_path_without_extension);

	append(bat_builder, "if %errorlevel% neq 0 exit /b %errorlevel%\n");
	append_format(bat_builder, R"(link "%.obj" /out:"%.exe" /entry:"main" /subsystem:console)", source_path_without_extension, source_path_without_extension);
	for (auto library : extern_libraries) {
		append_format(bat_builder, " %", library);
	}

	auto bat_path = to_pathchars(concatenate(executable_directory, "\\nasm_build.bat"s));
	write_entire_file(bat_path, as_bytes(to_string(bat_builder)));

	timed_block("nasm + link"s);

	auto process = start_process(bat_path);
	if (!process.handle) {
		print(Print_error, "Cannot execute file '%'\n", bat_path);
		return;
	}

	defer { free(process); };

	StringBuilder compile_log;

	while (1) {
		u8 buf[256];
		auto bytes_read = process.standard_out->read(array_as_span(buf));

		if (bytes_read == 0)
			break;

		auto string = Span((utf8 *)buf, bytes_read);
		print(string);
		append(compile_log, string);
	}

	write_entire_file("compile_log.txt"s, as_bytes(to_string(compile_log)));

	wait(process);
	auto exit_code = get_exit_code(process);
	if (exit_code != 0) {
		print(Print_error, "Build command failed\n");
		return;
	}

	print("Build succeeded\n");
}
