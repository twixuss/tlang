#include "nasm.h"
#include "ast.h"
#include "extern.h"

Span<utf8> cmov_string(Comparison c) {
	using enum Comparison;
	switch (c) {
		case e : return u8"e"s;
		case ne: return u8"ne"s;
		case l : return u8"l"s;
		case le: return u8"le"s;
		case g : return u8"g"s;
		case ge: return u8"ge"s;
	}
	invalid_code_path();
}

Span<utf8> locate_msvc() {
	Span<utf8> path = u8"C:\\Program Files (x86)\\Microsoft Visual Studio\\"s;

	auto find_version = [&]() -> Span<utf8> {
		Span<utf8> supported_versions[] = {
			u8"2022"s,
			u8"2019"s,
			u8"2017"s,
		};
		auto found_versions = get_items_in_directory(path);
		for (auto supported_version : supported_versions) {
			for (auto found_version : found_versions) {
				if (found_version.kind == FileItem_directory) {
					auto name = to_utf8(found_version.name);
					if (name == supported_version) {
						return format(u8"%%", path, name);
					}
				}
			}
		}
		return {};
	};
	auto find_edition = [&]() -> Span<utf8> {
		auto found_editions = get_items_in_directory(path);
		Span<utf8> supported_editions[] = {
			u8"Community"s,
			u8"Professional"s,
		};
		for (auto supported_edition : supported_editions) {
			for (auto found_edition : found_editions) {
				if (found_edition.kind == FileItem_directory) {
					auto name = to_utf8(found_edition.name);
					if (name == supported_edition) {
						return format(u8"%\\%\\VC\\Tools\\MSVC\\", path, name);
					}
				}
			}
		}
		return {};
	};


	path = find_version();
	if (!path.data) {
		return {};
	}
	path = find_edition();
	if (!path.data) {
		return {};
	}

	auto found_builds = get_items_in_directory(path);
	if (!found_builds.count) {
		return {};
	}
	path = format(u8"%%\\bin\\Hostx64\\x64\\", path, found_builds.back().name);

	return path;
}

Span<utf8> locate_wkits() {
	Span<utf8> path = u8"C:\\Program Files (x86)\\Windows Kits\\"s;

	auto find_version = [&]() -> Span<utf8> {
		Span<utf8> supported_versions[] = {
			u8"10"s,
			u8"8.1"s,
			u8"7"s,
		};
		auto found_versions = get_items_in_directory(path);
		for (auto supported_version : supported_versions) {
			for (auto found_version : found_versions) {
				if (found_version.kind == FileItem_directory) {
					auto name = to_utf8(found_version.name);
					if (name == supported_version) {
						return format(u8"%%\\Lib\\", path, name);
					}
				}
			}
		}
		return {};
	};

	path = find_version();
	if (!path.data) {
		return {};
	}

	auto found_builds = get_items_in_directory(path);
	if (!found_builds.count) {
		return {};
	}
	path = format(u8"%%\\um\\x64", path, found_builds.back().name); // NO SLASH AT THE END BECAUSE LINK.EXE IS ...

	return path;
}

void output_nasm(Bytecode &bytecode) {
	timed_function();


	StringBuilder builder;

	append(builder, "bits 64\n");

	for (auto f : bytecode.extern_functions) {
		append_format(builder, "extern %\n", f);
	}

	append(builder, "section .rodata\nconstants: db ");
	for (auto byte : bytecode.constant_data) {
		append_format(builder, "%,", byte);
	}
	append(builder, '\n');
	append(builder, "section .data\ndata: db ");
	for (auto byte : bytecode.data) {
		append_format(builder, "%,", byte);
	}
	append(builder, '\n');
	append(builder, "section .bss\nzeros: db ");
	for (auto byte : bytecode.zero_data) {
		append_format(builder, "%,", byte);
	}
	append(builder, '\n');

	auto l = [](auto val) { return FormatInt{.value=val, .radix=62}; };
	append_format(builder, "section .text\nglobal main\nmain:jmp .%\n", l(main_lambda->location_in_bytecode));

	s64 idx = 0;
	for (auto i : bytecode.instructions) {
		switch (i.kind) {
			using enum InstructionKind;
			case mov_rr:		append_format(builder, ".%: mov         %, %\n"                , l(idx), i.mov_rr.d, i.mov_rr.s); break;
			case mov_rc:		append_format(builder, ".%: mov         %, %\n"                , l(idx), i.mov_rc.d, i.mov_rc.s); break;
			case mov_rm:		append_format(builder, ".%: mov         %, qword [%]\n"        , l(idx), i.mov_rm.d, i.mov_rm.s); break;
			case mov_mr:		append_format(builder, ".%: mov         qword [%], %\n"        , l(idx), i.mov_mr.d, i.mov_mr.s); break;
			case push_r:		append_format(builder, ".%: push        %\n"                   , l(idx), i.push_r.s); break;
			case push_c:		append_format(builder, ".%: push        %\n"                   , l(idx), i.push_c.s); break;
			case push_m:		append_format(builder, ".%: push        qword [%]\n"           , l(idx), i.push_m.s); break;
			case pushcda:		append_format(builder, ".%: mov rax, constants + %\npush rax\n", l(idx), i.pushcda.s); break;
			case pushda:		append_format(builder, ".%: mov rax, data + %\npush rax\n"     , l(idx), i.pushda.s); break;
			case pushuda:		append_format(builder, ".%: mov rax, zeros + %\npush rax\n"    , l(idx), i.pushuda.s); break;
			case pop_r:			append_format(builder, ".%: pop         %\n"                   , l(idx), i.pop_r.d); break;
			case ret:			append_format(builder, ".%: ret\n"                             , l(idx)); break;
			case shl_rc:		append_format(builder, ".%: shl         %, %\n"                , l(idx), i.shl_rc.d, i.shl_rc.s); break;
			case add_rc:		append_format(builder, ".%: add         %, %\n"                , l(idx), i.add_rc.d, i.add_rc.s); break;
			case add_mc:		append_format(builder, ".%: add         qword [%], %\n"        , l(idx), i.add_mc.d, i.add_mc.s); break;
			case add_mr:		append_format(builder, ".%: add         qword [%], %\n"        , l(idx), i.add_mr.d, i.add_mr.s); break;
			case add_rr:		append_format(builder, ".%: add         %, %\n"                , l(idx), i.add_rr.d, i.add_rr.s); break;
			case sub_rc:		append_format(builder, ".%: sub         %, %\n"                , l(idx), i.sub_rc.d, i.sub_rc.s); break;
			case sub_rr:		append_format(builder, ".%: sub         %, %\n"                , l(idx), i.sub_rr.d, i.sub_rr.s); break;
			case sub_mr:		append_format(builder, ".%: sub         qword [%], %\n"        , l(idx), i.sub_mr.d, i.sub_mr.s); break;
			case mul_rc:		append_format(builder, ".%: imul        %, %\n"                , l(idx), i.mul_rc.d, i.mul_rc.s); break;
			case mul_mr: 		append_format(builder, ".%: imul        %, qword [%]\nmov qword [%], %\n", l(idx), i.mul_mr.s, i.mul_mr.d, i.mul_mr.d, i.mul_mr.s); break;
			case div_mr:		append_format(builder, ".%: div         qword [%], %\n"        , l(idx), i.div_mr.d, i.div_mr.s); break;
			case mod_mr:		append_format(builder, ".%: mod         qword [%], %\n"        , l(idx), i.mod_mr.d, i.mod_mr.s); break;
			case or_mr:			append_format(builder, ".%:  or         qword [%], %\n"        , l(idx), i. or_mr.d, i. or_mr.s); break;
			case and_rc:		append_format(builder, ".%: and         %, %\n"                , l(idx), i.and_rc.d, i.and_rc.s); break;
			case and_mr:		append_format(builder, ".%: and         qword [%], %\n"        , l(idx), i.and_mr.d, i.and_mr.s); break;
			case xor_rr:		append_format(builder, ".%: xor         %, %\n"                , l(idx), i.xor_rr.d, i.xor_rr.s); break;
			case xor_mr:		append_format(builder, ".%: xor         qword [%], %\n"        , l(idx), i.xor_mr.d, i.xor_mr.s); break;
			case cmp_rax_rbx:	append_format(builder, ".%: push 1\nmov %, 0\ncmp rax, rbx\ncmov% %, qword [rsp]\nadd rsp, 8\n", l(idx), i.cmp_rax_rbx.dst_reg, cmov_string(i.cmp_rax_rbx.comparison), i.cmp_rax_rbx.dst_reg); break;
			case call_constant:	append_format(builder, ".%: call        .%\n"                  , l(idx), l(i.call_constant.constant)); break;
			case call_string:	append_format(builder, ".%: call        %\n"                   , l(idx), i.call_string.string); break;
			case jmp:			append_format(builder, ".%: jmp         .%\n"                  , l(idx), l(idx + i.jmp.offset)); break;
			case jz:			append_format(builder, ".%: test %, %\njz .%\n"                , l(idx), i.jz.reg, i.jz.reg, l(idx + i.jz.offset)); break;
			default:invalid_code_path();
		}
		if (i.comment) {
			append_format(builder, "; %\n", i.comment);
		}
		++idx;
	}


	auto output_path = to_pathchars(format(u8"%.asm", source_path_without_extension));

	{
		auto file = open_file(output_path, {.write = true});
		defer { close(file); };

		write(file, as_bytes(to_string(builder)));
	}

	auto msvc_directory = locate_msvc();
	if (!msvc_directory.data) {
		print(Print_error, "Couldn't locate msvc");
		return;
	}

	auto wkits_directory = locate_wkits();
	if (!wkits_directory.data) {
		print(Print_error, "Couldn't locate windows kits");
		return;
	}

	StringBuilder bat_builder;
	append_format(bat_builder, u8R"(@echo off
%\nasm -f win64 -g "%.asm" -o "%.obj"
)", executable_directory, source_path_without_extension, source_path_without_extension);

	append(bat_builder, "if %errorlevel% neq 0 exit /b %errorlevel%\n");
	append_format(bat_builder, R"("%link" "%.obj" /out:"%.exe" /entry:"main" /subsystem:console /LIBPATH:"%")", msvc_directory, source_path_without_extension, source_path_without_extension, wkits_directory);
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
