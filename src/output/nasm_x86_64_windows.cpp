#include <bytecode.h>
#include <ast.h>

static Span<utf8> cmov_string(Comparison c) {
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

static Span<utf8> cmps_string(Comparison c) {
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

static Span<utf8> cmpu_string(Comparison c) {
	using enum Comparison;
	switch (c) {
		case e : return u8"e"s;
		case ne: return u8"ne"s;
		case l : return u8"b"s;
		case le: return u8"be"s;
		case g : return u8"a"s;
		case ge: return u8"ae"s;
	}
	invalid_code_path();
}

static umm append(StringBuilder &builder, Register r) {
	using enum Register;
	switch (r) {
		case r0: return append(builder, "r8");
		case r1: return append(builder, "r9");
		case r2: return append(builder, "r10");
		case r3: return append(builder, "r11");
		case r4: return append(builder, "r12");
		case r5: return append(builder, "r13");
		case r6: return append(builder, "r14");
		case r7: return append(builder, "r15");
		case rs:  return append(builder, "rsp");
		case rb:  return append(builder, "rbp");
		default:
			invalid_code_path();
	}
}

static umm append(StringBuilder &builder, Address a) {
	assert(!a.register_offset_scale);

	umm result = 0;
	auto append_ = [&](auto &b, auto v) {
		result += append(b, v);
	};

	append_(builder, '[');
	append_(builder, a.base);
	if (a.constant_offset) {
		append_(builder, '+');
		append_(builder, a.constant_offset);
	}
	append_(builder, ']');
	return result;
}

static umm append(StringBuilder &builder, XRegister r) {
	using enum XRegister;
	switch (r) {
		case x0: return append(builder, "xmm0");
		case x1: return append(builder, "xmm1");
		case x2: return append(builder, "xmm2");
		case x3: return append(builder, "xmm3");
		default:
			invalid_code_path();
	}
}

static Span<utf8> locate_msvc() {
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
						return format(u8"{{}", path, name);
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
						return format(u8"{}\\{}\\VC\\Tools\\MSVC\\", path, name);
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
	path = format(u8"{{}\\bin\\Hostx64\\x64\\", path, found_builds.back().name);

	return path;
}

static Span<utf8> locate_wkits() {
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
						return format(u8"{{}\\Lib\\", path, name);
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
	path = format(u8"{{}\\um\\x64", path, found_builds.back().name); // NO SLASH AT THE END BECAUSE LINK.EXE IS ...

	return path;
}

static void append_instructions(StringBuilder &builder, List<Instruction> instructions) {
	timed_function();

	auto l = [](auto val) { return FormatInt{.value=val, .radix=62}; };
	append_format(builder, "section .text\nglobal main\nmain:\npush 0\ncall .{}\npop rcx\nand rsp, -16\nsub rsp, 16\ncall ExitProcess\nret\n", l(main_lambda->location_in_bytecode));

	auto part1b = [&](Register r) {
		switch (r) {
			case Register::rb: return u8"bpl"s;
			case Register::rs: return u8"spl"s;
			case Register::r0:  return u8"r8b"s;
			case Register::r1:  return u8"r9b"s;
			case Register::r2: return u8"r10b"s;
			case Register::r3: return u8"r11b"s;
			case Register::r4: return u8"r12b"s;
			case Register::r5: return u8"r13b"s;
			case Register::r6: return u8"r14b"s;
			case Register::r7: return u8"r15b"s;
			default: invalid_code_path();
		}
	};
	auto part2b = [&](Register r) {
		switch (r) {
			case Register::rb: return u8"bp"s;
			case Register::rs: return u8"sp"s;
			case Register::r0:  return u8"r8w"s;
			case Register::r1:  return u8"r9w"s;
			case Register::r2: return u8"r10w"s;
			case Register::r3: return u8"r11w"s;
			case Register::r4: return u8"r12w"s;
			case Register::r5: return u8"r13w"s;
			case Register::r6: return u8"r14w"s;
			case Register::r7: return u8"r15w"s;
			default: invalid_code_path();
		}
	};
	auto part4b = [&](Register r) {
		switch (r) {
			case Register::rb: return u8"ebp"s;
			case Register::rs: return u8"esp"s;
			case Register::r0:  return u8"r8d"s;
			case Register::r1:  return u8"r9d"s;
			case Register::r2: return u8"r10d"s;
			case Register::r3: return u8"r11d"s;
			case Register::r4: return u8"r12d"s;
			case Register::r5: return u8"r13d"s;
			case Register::r6: return u8"r14d"s;
			case Register::r7: return u8"r15d"s;
			default: invalid_code_path();
		}
	};
	auto part8b = [&](Register r) {
		return r;
	};

	auto move_stdcall_registers = [&] {
		append(builder, "mov rcx, r8\nmov rdx, r9\nmov r8, r10\nmov r9, r11\n");
	};

	s64 idx = 0;
	for (auto i : instructions) {
		append_format(builder, ".{}: ", l(idx));
		switch (i.kind) {
			using enum InstructionKind;
			case mov_rr: append_format(builder, "mov {}, {}", i.mov_rr.d, i.mov_rr.s); break;
			case mov_rc: append_format(builder, "mov {}, {}", i.mov_rc.d, i.mov_rc.s); break;

			case mov1_mc: append_format(builder, "mov byte {}, {}", i.mov1_mc.d, i.mov1_mc.s); break;
			case mov2_mc: append_format(builder, "mov word {}, {}", i.mov2_mc.d, i.mov2_mc.s); break;
			case mov4_mc: append_format(builder, "mov dword {}, {}", i.mov4_mc.d, i.mov4_mc.s); break;
			case mov8_mc: append_format(builder, "mov qword {}, {}", i.mov8_mc.d, i.mov8_mc.s); break;

			case mov1_rm: append_format(builder, "mov {}, byte {}", part1b(i.mov1_rm.d), i.mov1_rm.s); break;
			case mov2_rm: append_format(builder, "mov {}, word {}", part2b(i.mov2_rm.d), i.mov2_rm.s); break;
			case mov4_rm: append_format(builder, "mov {}, dword {}", part4b(i.mov4_rm.d), i.mov4_rm.s); break;
			case mov8_rm: append_format(builder, "mov {}, qword {}", part8b(i.mov8_rm.d), i.mov8_rm.s); break;

			case mov1_mr: append_format(builder, "mov byte {}, {}", i.mov1_mr.d, part1b(i.mov1_mr.s)); break;
			case mov2_mr: append_format(builder, "mov word {}, {}", i.mov2_mr.d, part2b(i.mov2_mr.s)); break;
			case mov4_mr: append_format(builder, "mov dword {}, {}", i.mov4_mr.d, part4b(i.mov4_mr.s)); break;
			case mov8_mr: append_format(builder, "mov qword {}, {}", i.mov8_mr.d, part8b(i.mov8_mr.s)); break;

			case push_r: append_format(builder, "push {}", i.push_r.s); break;
			case push_c:
				if (i.push_c.s > max_value<u32>) {
					append_format(builder, "push dword {}\nmov dword [rsp+4], {}", (s32)i.push_c.s, (s32)(i.push_c.s >> 32));
				} else {
					append_format(builder, "push {}", (s32)i.push_c.s);
				}
				break;
			case push_m: append_format(builder, "push        qword {}"           , i.push_m.s); break;

			case pushcda:    append_format(builder, "mov rax, constants + {}\npush rax", i.pushcda.s); break;
			case pushda:     append_format(builder, "mov rax, data + {}\npush rax"     , i.pushda.s); break;
			case pushuda:    append_format(builder, "mov rax, zeros + {}\npush rax"    , i.pushuda.s); break;
			case pushta:     append_format(builder, "mov rax, .{}\npush rax"           , l(i.pushta.s)); break;
			case pushextern: append_format(builder, "mov rax, {}\npush rax"            , i.pushextern.s); break;

			case pop_r: append_format(builder, "pop {}", i.pop_r.d); break;

			case ret: append_format(builder, "ret"); break;

			case shl_rc: append_format(builder, "shl {}, {}", i.shl_rc.d, i.shl_rc.s); break;
			case shl_mr: append_format(builder, "mov cl, {}\nshl qword {}, cl", part1b(i.shl_mr.s), i.shl_mr.d); break;

			case shr_rc: append_format(builder, "shr {}, {}", i.shr_rc.d, i.shr_rc.s); break;
			case shr_mr: append_format(builder, "mov cl, {}\nshr qword {}, cl", part1b(i.shr_mr.s), i.shr_mr.d); break;

			case add_rc: append_format(builder, "add {}, {}"        , i.add_rc.d, i.add_rc.s); break;
			case add_rr: append_format(builder, "add {}, {}"        , i.add_rr.d, i.add_rr.s); break;
			case add_mc: append_format(builder, "add qword {}, {}", i.add_mc.d, i.add_mc.s); break;
			case add_mr: append_format(builder, "add qword {}, {}", i.add_mr.d, i.add_mr.s); break;

			case sub_rc: append_format(builder, "sub {}, {}"        , i.sub_rc.d, i.sub_rc.s); break;
			case sub_rr: append_format(builder, "sub {}, {}"        , i.sub_rr.d, i.sub_rr.s); break;
			case sub_mc: append_format(builder, "sub qword {}, {}", i.sub_mc.d, i.sub_mc.s); break;
			case sub_mr: append_format(builder, "sub qword {}, {}", i.sub_mr.d, i.sub_mr.s); break;

			case mul_rc: append_format(builder, "imul {}, {}", i.mul_rc.d, i.mul_rc.s); break;
			case mul_mr: append_format(builder, "imul {}, qword {}\nmov qword {}, {}", i.mul_mr.s, i.mul_mr.d, i.mul_mr.d, i.mul_mr.s); break;

			case div_mr: append_format(builder, "mov rdx, 0\nmov rax, qword {}\ndiv {}\nmov qword {}, rax", i.div_mr.d, i.div_mr.s, i.div_mr.d); break;

			case mod_mr: append_format(builder, "mov rdx, 0\nmov rax, qword {}\ndiv {}\nmov qword {}, rdx", i.mod_mr.d, i.mod_mr.s, i.mod_mr.d); break;

			case or_mr: append_format(builder, "or qword {}, {}", i. or_mr.d, i. or_mr.s); break;

			case and_rc: append_format(builder, "and {}, {}"        , i.and_rc.d, i.and_rc.s); break;
			case and_mc: append_format(builder, "and qword {}, {}", i.and_mc.d, i.and_mc.s); break;
			case and_mr: append_format(builder, "and qword {}, {}", i.and_mr.d, i.and_mr.s); break;

			case xor_rr: append_format(builder, "xor {}, {}"        , i.xor_rr.d, i.xor_rr.s); break;
			case xor_mr: append_format(builder, "xor qword {}, {}", i.xor_mr.d, i.xor_mr.s); break;

			case cmps1: append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmps1.d, i.cmps1.d, part1b(i.cmps1.a), part1b(i.cmps1.b), cmps_string(i.cmps1.c), part1b(i.cmps1.d)); break;
			case cmps2: append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmps2.d, i.cmps2.d, part2b(i.cmps2.a), part2b(i.cmps2.b), cmps_string(i.cmps2.c), part1b(i.cmps2.d)); break;
			case cmps4: append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmps4.d, i.cmps4.d, part4b(i.cmps4.a), part4b(i.cmps4.b), cmps_string(i.cmps4.c), part1b(i.cmps4.d)); break;
			case cmps8: append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmps8.d, i.cmps8.d, part8b(i.cmps8.a), part8b(i.cmps8.b), cmps_string(i.cmps8.c), part1b(i.cmps8.d)); break;
			case cmpu1: append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmpu1.d, i.cmpu1.d, part1b(i.cmpu1.a), part1b(i.cmpu1.b), cmpu_string(i.cmpu1.c), part1b(i.cmpu1.d)); break;
			case cmpu2: append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmpu2.d, i.cmpu2.d, part2b(i.cmpu2.a), part2b(i.cmpu2.b), cmpu_string(i.cmpu2.c), part1b(i.cmpu2.d)); break;
			case cmpu4: append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmpu4.d, i.cmpu4.d, part4b(i.cmpu4.a), part4b(i.cmpu4.b), cmpu_string(i.cmpu4.c), part1b(i.cmpu4.d)); break;
			case cmpu8: append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmpu8.d, i.cmpu8.d, part8b(i.cmpu8.a), part8b(i.cmpu8.b), cmpu_string(i.cmpu8.c), part1b(i.cmpu8.d)); break;

			case popcall: append(builder, "pop rax\ncall rax"); break;
			case call_constant: append_format(builder, "call .{}", l(i.call_constant.constant)); break;
			case call_string:   append_format(builder, "call {}", i.call_string.string); break;

			case jmp: append_format(builder, "jmp .{}", l(idx + i.jmp.offset)); break;

			case jz: append_format(builder, "test {}, {}\njz .{}", i.jz.reg, i.jz.reg, l(idx + i.jz.offset)); break;

			case copyf_mmc:
				append_format(builder, "mov rcx, {}\nmov rsi, {}\nmov rdi, {}\ncld\nrep movsb", i.copyf_mmc.size, i.copyf_mmc.s, i.copyf_mmc.d);
				break;
			case copyb_mmc:
				append_format(builder, "mov rcx, {}\nlea rsi, [{} + {}]\nlea rdi, [{} + {}]\nstd\nrep movsb", i.copyb_mmc.size, i.copyb_mmc.s, i.copyb_mmc.size - 1, i.copyb_mmc.d, i.copyb_mmc.size - 1);
				break;

			case copyf_ssc:
				append_format(builder, "mov rcx, {}\npop rsi\npop rdi\ncld\nrep movsb", i.copyf_ssc.size);
				break;
			case copyb_ssc:
				append_format(builder, "mov rcx, {}\npop rsi\npop rdi\nadd rsi, {}\nadd rdi, {}\nstd\nrep movsb", i.copyb_ssc.size, i.copyb_ssc.size - 1, i.copyb_ssc.size - 1);
				break;

			case set_mcc:
				append_format(builder, "mov rdi, {}\nmov al, {}\nmov rcx, {}\ncld\nrep stosb", i.set_mcc.d, i.set_mcc.s, i.set_mcc.size);
				break;

			case stdcall_begin_lambda: {
				auto lambda = i.stdcall_begin_lambda.lambda;
				append_format(builder, "sub rsp, {}\n", ceil(get_size(lambda->return_parameter->type), 8ll));

				// push parameters
				if (lambda->parameters.count >= 4) append(builder, "push r9\n");
				if (lambda->parameters.count >= 3) append(builder, "push r8\n");
				if (lambda->parameters.count >= 2) append(builder, "push rdx\n");
				if (lambda->parameters.count >= 1) append(builder, "push rcx\n");

				assert(lambda->parameters.count <= 4, "not implemented");

				// Dummy return address
				append(builder, "push 0x12345678\npush rbp\nmov rbp, rsp");
				break;
			}
			case stdcall_end_lambda: {
				auto lambda = i.stdcall_end_lambda.lambda;
				// pop fake return address and parameters
				append_format(builder, "add rsp, {}\npop rax\nmov rsp, rbp\npop rbp", 8 + lambda->parameters_size);
				break;
			}

			case popstdcall:
				move_stdcall_registers();
				append(builder, "pop rax\ncall rax");
				break;
			case stdcall_constant: {
				move_stdcall_registers();
				append_format(builder, "call .{}", l(i.stdcall_constant.constant));
				break;
			}
			case stdcall_string: {
				move_stdcall_registers();
				append_format(builder, "call {}", i.stdcall_string.string);
				break;
			}

			case push_stdcall_result: append(builder, "push rax"); break;

			case stdcall_m:
				move_stdcall_registers();
				append_format(builder, "call qword {}", i.stdcall_m.s);
				break;

			case call_m:
				append_format(builder, "call qword {}", i.call_m.s);
				break;

			case lea:
				append_format(builder, "lea {}, {}", i.lea.d, i.lea.s);
				break;

			case cvtf64s64:
				append(builder, "cvtsd2si rax, [rsp]\nmov [rsp], rax");
				break;

			case mov_f64r: append_format(builder, "movq {}, {}", i.mov_f64r.d, i.mov_f64r.s); break;
	        case mov_rf64: append_format(builder, "movq {}, {}", i.mov_rf64.d, i.mov_rf64.s); break;

	        case add_f64: append_format(builder, "addsd {}, {}", i.add_f64.d, i.add_f64.s); break;
	        case sub_f64: append_format(builder, "subsd {}, {}", i.sub_f64.d, i.sub_f64.s); break;
	        case mul_f64: append_format(builder, "mulsd {}, {}", i.mul_f64.d, i.mul_f64.s); break;
	        case div_f64: append_format(builder, "divsd {}, {}", i.div_f64.d, i.div_f64.s); break;

			default:invalid_code_path();
		}
#if BYTECODE_DEBUG
		append_format(builder, "; bytecode.cpp:{}\n", i.line);
		if (i.comment) {
			append_format(builder, "; {}\n", i.comment);
		}
#else
		append(builder, '\n');
#endif
		++idx;
	}
}

void output_nasm_x86_64_windows(Bytecode &bytecode) {
	timed_function();


	StringBuilder builder;

	append(builder, "bits 64\nextern ExitProcess\n");

	for_each(bytecode.extern_libraries, [&](auto lib, auto fns) {
		for (auto f : fns) {
			append_format(builder, "extern {}\n", f);
		}
	});

	append(builder, "section .rodata\nconstants: db ");
	for (auto byte : bytecode.constant_data) {
		append_format(builder, "{},", byte);
	}
	append(builder, '\n');
	append(builder, "section .data\ndata: db ");
	for (auto byte : bytecode.data) {
		append_format(builder, "{},", byte);
	}
	append_format(builder, "\nsection .bss\nzeros: resb {}\n", bytecode.zero_data_size);

	append_instructions(builder, bytecode.instructions);

	auto output_path_base = format("{}\\{}", current_directory, parse_path(source_path).name);

	auto asm_path = to_pathchars(format(u8"{}.asm", output_path_base));

	{
		auto file = open_file(asm_path, {.write = true});
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
{}\nasm -f win64 -gcv8 "{}.asm" -o "{}.obj" -w-number-overflow -w-db-empty
)", executable_directory, output_path_base, output_path_base);

	append(bat_builder, "if %errorlevel% neq 0 exit /b %errorlevel%\n");
	append_format(bat_builder,
		R"("{}link" "{}.obj" /out:"{}.exe" /nodefaultlib /entry:"main" /subsystem:console /DEBUG:FULL /LIBPATH:"{}" kernel32.lib)",
		msvc_directory,
		output_path_base,
		output_path_base,
		wkits_directory
	);
	for_each(bytecode.extern_libraries, [&](auto library, auto) {
		append_format(bat_builder, " {}.lib", library);
	});

	auto bat_path = to_pathchars(concatenate(executable_directory, u8"\\nasm_build.bat"s));
	write_entire_file(bat_path, as_bytes(to_string(bat_builder)));

	timed_block("nasm + link"s);

	auto process = start_process(bat_path);
	if (!process.handle) {
		print(Print_error, "Cannot execute file '{}'\n", bat_path);
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
