#define TL_IMPL
#pragma warning(disable: 4702) // unreachable
#include <bytecode.h>
#include <compiler.h>
#include "../x86_64.h"
#include "msvc.h"
#include "../coff.h"
#include <fadec-enc.h>
#pragma comment(lib, "encode.c.obj")

using namespace x86_64;

constexpr u8 rex_w = 0b0100'1000;
constexpr u8 rex_r = 0b0100'0100;
constexpr u8 rex_x = 0b0100'0010;
constexpr u8 rex_b = 0b0100'0001;

using xAddress = x86_64::Address;
using bAddress = ::Address;

inline constexpr bool is_gpr(Register64 r) { return (u8)r >= 8; }

auto e(u8 **buf, u64 mnem, s64 op0, s64 op1, s64 op2, s64 op3) { auto result = fe_enc64_impl(buf, mnem, op0, op1, op2, op3); assert(result == 0); return result; }
auto e(u8 **buf, u64 mnem, s64 op0, s64 op1, s64 op2) { auto result = fe_enc64_impl(buf, mnem, op0, op1, op2, 0); assert(result == 0); return result; }
auto e(u8 **buf, u64 mnem, s64 op0, s64 op1) { auto result = fe_enc64_impl(buf, mnem, op0, op1, 0, 0); assert(result == 0); return result; }
auto e(u8 **buf, u64 mnem, s64 op0) { auto result = fe_enc64_impl(buf, mnem, op0, 0, 0, 0); assert(result == 0); return result; }
auto e(u8 **buf, u64 mnem) { auto result = fe_enc64_impl(buf, mnem, 0, 0, 0, 0); assert(result == 0); return result; }

u64 set_fe_s(Comparison c) {
	switch (c) {
		using enum Comparison;
		case e:  return FE_SETZ8r;
		case ne: return FE_SETNZ8r;
		case l:	 return FE_SETL8r;
		case le: return FE_SETLE8r;
		case g:	 return FE_SETG8r;
		case ge: return FE_SETGE8r;
	}
	return 0;
}
u64 set_fe_u(Comparison c) {
	switch (c) {
		using enum Comparison;
		case e:  return FE_SETZ8r;
		case ne: return FE_SETNZ8r;
		case l:	 invalid_code_path();//return FE_SETB8r;
		case le: return FE_SETBE8r;
		case g:	 return FE_SETA8r;
		case ge: invalid_code_path();//return FE_SETAE8r;
	}
	return 0;
}

s64 saved_registers_size;
s64 temporary_offset;
s64 locals_offset;
s64 parameters_size;

auto fe(Register64 r) {
	switch (r) {
		using enum Register64;
		case rax: return FE_AX;
		case rcx: return FE_CX;
		case rdx: return FE_DX;
		case rbx: return FE_BX;
		case rsp: return FE_SP;
		case rbp: return FE_BP;
		case rsi: return FE_SI;
		case rdi: return FE_DI;
		case r8:  return FE_R8;
		case r9:  return FE_R9;
		case r10: return FE_R10;
		case r11: return FE_R11;
		case r12: return FE_R12;
		case r13: return FE_R13;
		case r14: return FE_R14;
		case r15: return FE_R15;
	}
	return (FeReg)0;
}
auto fe(Register r) {
	return fe(to_x86_register(r));
}
auto fe(::Address a) {
	switch (a.base) {
		using enum Register;
		case locals:
			a.base = rb;
			a.c += locals_offset;
			break;
		case temporary:
			a.base = rb;
			a.c += temporary_offset;
			break;
		case parameters:
			a.base = rb;
			a.c = parameters_size - a.c + 8;
			break;
		case return_parameters:
			a.base = rb;
			a.c += parameters_size + 16;
			break;
	}
	auto scale = lea_scales[a.r1_scale_index];
	return FE_MEM(fe(a.base), scale, scale ? fe(a.r1) : 0, a.c);
}

DECLARE_OUTPUT_BUILDER {
	init_allocator();
	current_printer = console_printer;

	timed_function(compiler->profiler);

	auto output_path_base = format("{}\\{}", compiler->current_directory, parse_path(compiler->source_path).name);
	auto obj_path = to_pathchars(format(u8"{}.obj", output_path_base));

	auto msvc_directory = locate_msvc();
	if (!msvc_directory.data) {
		with(ConsoleColor::red, print("Couldn't locate msvc"));
		return false;
	}

	auto wkits_directory = locate_wkits();
	if (!wkits_directory.data) {
		with(ConsoleColor::red, print("Couldn't locate windows kits"));
		return false;
	}

	{
		scoped_phase("Writing .obj");

		coff::Writer writer;
		writer.Machine = IMAGE_FILE_MACHINE_AMD64;

		writer.symbols.add({
			.name = ".file"str,
			.section = coff::Writer::debug_section,
			.StorageClass = IMAGE_SYM_CLASS_FILE,
			.NumberOfAuxSymbols = 1,
		});
		writer.symbols.add({
			.name = parse_path(compiler->source_path).name_and_extension(),
		});

		auto &text_section = writer.sections.add();
		text_section.name = ".text"str;
		text_section.Characteristics = IMAGE_SCN_ALIGN_64BYTES|IMAGE_SCN_CNT_CODE|IMAGE_SCN_MEM_READ|IMAGE_SCN_MEM_EXECUTE;

		auto &rodata_section = writer.sections.add();
		rodata_section.name = ".rodata"str;
		rodata_section.Characteristics = IMAGE_SCN_ALIGN_64BYTES|IMAGE_SCN_CNT_INITIALIZED_DATA|IMAGE_SCN_MEM_READ;

		StringBuilder text_builder;

		using enum Register64;

		struct ModRM {
			u8 rm  : 3;
			u8 reg : 3;
			u8 mod : 2;
		};

		List<coff::Writer::Relocation *> text_relocations, rodata_relocations;
		HashMap<String, List<coff::Writer::Relocation *>> external_relocations;
		// Opcoder o{text_builder, text_section, bytecode};
		// o.convert();

		HashMap<u32, u32> instruction_offsets;
		HashMap<u8 *, u32> local_relocations;

		auto buf = default_allocator.allocate<u8>(bytecode.instructions.count * 15);
		auto cur = buf;

		auto size = [&]() -> u32 {
			return cur - buf;
		};
		auto e = [&](auto ...args) {
			return ::e(&cur, args...);
		};

		e(FE_AND64ri, FE_SP, -16);
		e(FE_PUSHi, 0);
		e(FE_PUSHi, 0);
		e(FE_CLD);
		e(FE_CALL, (s64)cur+5);
		local_relocations.get_or_insert(cur-4) = compiler->main_lambda->location_in_bytecode;
		e(FE_MOV64rm, FE_CX, FE_MEM(FE_SP, 0, 0, 0));
		e(FE_CALL, (s64)cur+5);
		external_relocations.get_or_insert("ExitProcess"str).add(&text_section.relocations.add({.offset=size()-4,.type=IMAGE_REL_AMD64_REL32}));

		text_section.line_numbers.add({.type=(u32)writer.symbols.count, .line=0});
		writer.symbols.add({
			.name = "main"str,
			.section = &text_section,
			.StorageClass = IMAGE_SYM_CLASS_STATIC,
		});
		// auto line_number = default_allocator.allocate<utf8>(8);
		// *(u64 *)line_number = text_section.line_numbers.count-1;
		// writer.symbols.add({
		// 	.name = Span(line_number, 8),
		// });

		u32 idx = 0;
		for (auto &i : bytecode.instructions) {
			defer { ++idx; };
			switch (i.kind) {
				using enum InstructionKind;
				case jmp_label:
					instruction_offsets.get_or_insert(idx) = size();
					break;
				case debug_start_lambda: {
					/*
					text_section.line_numbers.add({.type=(u32)writer.symbols.count, .line=0});
					writer.symbols.add({
						.name = i.debug_start_lambda.lambda->definition ? i.debug_start_lambda.lambda->definition->name : i.debug_start_lambda.lambda->location,
						.section = &text_section,
						.StorageClass = IMAGE_SYM_CLASS_STATIC,
						.NumberOfAuxSymbols = 1,
					});
					auto line_number = default_allocator.allocate<utf8>(8);
					*(u64 *)line_number = text_section.line_numbers.count-1;
					writer.symbols.add({
						.name = Span(line_number, 8),
					});
					*/
					break;
				}
				case debug_line:
					// text_section.line_numbers.add({.type=size(), .line=(u16)i.debug_line.line});
					break;
				case noop:
					break;
				case push_c: {
					REDECLARE_REF(i, i.push_c);
					if (min_value<s32> <= i.s && i.s <= max_value<s32>) {
						e(FE_PUSHi, i.s);
					} else {
						e(FE_SUB64ri, FE_SP, 8);
						e(FE_MOV64mi, FE_MEM(FE_SP, 0, 0, 0), (s32)i.s);
						e(FE_MOV64mi, FE_MEM(FE_SP, 0, 0, 4), (s32)(i.s >> 32));
					}
					break;
				}
				case push_r: e(FE_PUSHr, fe(i.push_r.s)); break;
				case push_m: e(FE_PUSHm, fe(i.push_m.s)); break;
				case pop_r: e(FE_POPr, fe(i.pop_r.d)); break;

				case mov_rc: e(FE_MOV64ri, fe(i.mov_rc.d), i.mov_rc.s); break;
				case mov_rr: e(FE_MOV64rr, fe(i.mov_rr.d), fe(i.mov_rr.s)); break;
				case mov_re:
					e(FE_MOV64ri, fe(i.mov_re.d), 0xaaaaaaaaaaaaaaaa);
					((u64 *)cur)[-1] = 0;
					external_relocations.get_or_insert(i.mov_re.s).add(&text_section.relocations.add({.offset=size()-8,.type=IMAGE_REL_AMD64_ADDR64}));
					break;

				case mov1_rm: e(FE_MOV8rm , fe(i.mov1_rm.d), fe(i.mov1_rm.s)); break;
				case mov2_rm: e(FE_MOV16rm, fe(i.mov2_rm.d), fe(i.mov2_rm.s)); break;
				case mov4_rm: e(FE_MOV32rm, fe(i.mov4_rm.d), fe(i.mov4_rm.s)); break;
				case mov8_rm: e(FE_MOV64rm, fe(i.mov8_rm.d), fe(i.mov8_rm.s)); break;

				case mov1_mr: e(FE_MOV8mr , fe(i.mov1_mr.d), fe(i.mov1_mr.s)); break;
				case mov2_mr: e(FE_MOV16mr, fe(i.mov2_mr.d), fe(i.mov2_mr.s)); break;
				case mov4_mr: e(FE_MOV32mr, fe(i.mov4_mr.d), fe(i.mov4_mr.s)); break;
				case mov8_mr: e(FE_MOV64mr, fe(i.mov8_mr.d), fe(i.mov8_mr.s)); break;

				case xchg8_mr: e(FE_XCHG64mr, fe(i.xchg8_mr.a), fe(i.xchg8_mr.b)); break;

				case lea: e(FE_LEA64rm, fe(i.lea.d), fe(i.lea.s)); break;

				case negi_r: e(FE_NEG64r, fe(i.negi_r.d)); break;

				case sub_rr: e(FE_SUB64rr, fe(i.sub_rr.d), fe(i.sub_rr.s)); break;
				case sub_rc: e(FE_SUB64ri, fe(i.sub_rc.d), i.sub_rc.s); break;

				case add_rr: e(FE_ADD64rr, fe(i.add_rr.d), fe(i.add_rr.s)); break;
				case add_rc: e(FE_ADD64ri, fe(i.add_rc.d), i.add_rc.s); break;

				case copyf_mmc: {
					REDECLARE_REF(i, i.copyf_mmc);
					e(FE_LEA64rm, FE_SI, fe(i.s));
					e(FE_LEA64rm, FE_DI, fe(i.d));
					e(FE_MOV64ri, FE_CX, i.size);
					e(FE_REP_MOVS8);
					break;
				}

				case jmp: {
					e(FE_JMP | FE_JMPL, (s64)cur);
					local_relocations.get_or_insert(cur-4) = idx + i.jmp.offset;
					break;
				}
				case jz_cr: {
					e(FE_TEST8rr, fe(i.jz_cr.reg), fe(i.jz_cr.reg));
					e(FE_JZ | FE_JMPL, (s64)cur);
					local_relocations.get_or_insert(cur-4) = idx + i.jz_cr.offset;
					break;
				}

				case call_c:
					e(FE_CALL, (s64)cur);
					local_relocations.get_or_insert(cur-4) = i.call_c.constant;
					break;
				case call_r: e(FE_CALLr, fe(i.call_r.s)); break;
				case ret: e(FE_RET); break;

				case cmps1: { REDECLARE_REF(i, i.cmps1); e(FE_XOR64rr, fe(i.d), fe(i.d)); e(FE_CMP8rr,  fe(i.a), fe(i.b)); e(set_fe_s(i.c), fe(i.d)); break; }
				case cmps2: { REDECLARE_REF(i, i.cmps2); e(FE_XOR64rr, fe(i.d), fe(i.d)); e(FE_CMP16rr, fe(i.a), fe(i.b)); e(set_fe_s(i.c), fe(i.d)); break; }
				case cmps4: { REDECLARE_REF(i, i.cmps4); e(FE_XOR64rr, fe(i.d), fe(i.d)); e(FE_CMP32rr, fe(i.a), fe(i.b)); e(set_fe_s(i.c), fe(i.d)); break; }
				case cmps8: { REDECLARE_REF(i, i.cmps8); e(FE_XOR64rr, fe(i.d), fe(i.d)); e(FE_CMP64rr, fe(i.a), fe(i.b)); e(set_fe_s(i.c), fe(i.d)); break; }
				case cmpu1: { REDECLARE_REF(i, i.cmpu1); e(FE_XOR64rr, fe(i.d), fe(i.d)); e(FE_CMP8rr,  fe(i.a), fe(i.b)); e(set_fe_u(i.c), fe(i.d)); break; }
				case cmpu2: { REDECLARE_REF(i, i.cmpu2); e(FE_XOR64rr, fe(i.d), fe(i.d)); e(FE_CMP16rr, fe(i.a), fe(i.b)); e(set_fe_u(i.c), fe(i.d)); break; }
				case cmpu4: { REDECLARE_REF(i, i.cmpu4); e(FE_XOR64rr, fe(i.d), fe(i.d)); e(FE_CMP32rr, fe(i.a), fe(i.b)); e(set_fe_u(i.c), fe(i.d)); break; }
				case cmpu8: { REDECLARE_REF(i, i.cmpu8); e(FE_XOR64rr, fe(i.d), fe(i.d)); e(FE_CMP64rr, fe(i.a), fe(i.b)); e(set_fe_u(i.c), fe(i.d)); break; }

				case begin_lambda: {
					REDECLARE_REF(i, i.begin_lambda);

					auto lambda = i.lambda;

					switch (lambda->convention) {
						case CallingConvention::tlang: {
							e(FE_PUSHr, FE_BP);
							e(FE_MOV64rr, FE_BP, FE_SP);

							saved_registers_size = 0;

							for_each(lambda->used_registers, [&](umm bit) {
								e(FE_PUSHr, fe((Register)bit));
								saved_registers_size += 8;
							});

							// keep the stack 16-byte aligned
							if (lambda->used_registers.count() & 1) {
								e(FE_SUB64ri, FE_SP, 8);
								saved_registers_size += 8;
							}


							auto used_bytes = lambda->locals_size + lambda->temporary_size + lambda->max_stack_space_used_for_call;
							if (used_bytes >= 4096) {
								e(FE_MOV64ri, FE_AX, used_bytes);
								e(FE_CALL, (s64)cur+5);
								// local_relocations.get_or_insert(cur-4) = index_of(bytecode.instructions, );
								invalid_code_path("call ._ps");
							}

							if (used_bytes) {
								e(FE_SUB64ri, FE_SP, used_bytes);
							}

							temporary_offset = -(saved_registers_size + lambda->temporary_size);
							locals_offset    = -(saved_registers_size + lambda->temporary_size + lambda->locals_size);
							parameters_size  = lambda->parameters_size;

							//not_implemented();
							break;
						}
						case CallingConvention::stdcall: not_implemented();
					}
					break;
				}
				case end_lambda: {
					REDECLARE_REF(i, i.end_lambda);
					auto lambda = i.lambda;
					switch (lambda->convention) {
						case CallingConvention::tlang: {

							auto used_bytes = lambda->locals_size + lambda->temporary_size + lambda->max_stack_space_used_for_call;

							// keep the stack 16-byte aligned
							if (lambda->used_registers.count() & 1) {
								used_bytes += 8;
							}
							if (used_bytes)
								e(FE_ADD64ri, FE_SP, used_bytes);

							for_each(lambda->used_registers, [&](umm bit) {
								e(FE_POPr, fe((Register)bit));
							});

							e(FE_MOV64rr, FE_SP, FE_BP);
							e(FE_POPr, FE_BP);
							e(FE_RET);
							break;
						}
						case CallingConvention::stdcall: not_implemented();
					}
					break;
				}
				default:
					invalid_code_path();
			}
		}

		for_each(local_relocations, [&](auto addr, auto instr_idx) {
			auto target = buf + instruction_offsets.find(instr_idx)->value;
			*(u32 *)addr = target - addr - 4;
		});


		text_section.data = {buf, (umm)(cur-buf)};

		not_implemented();//
		// assert(bytecode.constant_data_builder.parts.count == 1);
		// rodata_section.data = to_list(bytecode.constant_data_builder.parts[0].builder);

		auto &text_symbol = writer.symbols.add({
			.name = ".text"str,
			.section = &text_section,
			.StorageClass = IMAGE_SYM_CLASS_STATIC,
			.NumberOfAuxSymbols = 1,
		});
		utf8 text_section_size[8];
		*(u64 *)text_section_size = text_section.data.count;
		writer.symbols.add({
			.name = text_section_size,
		});

		auto &rodata_symbol = writer.symbols.add({
			.name = ".rodata"str,
			.section = &rodata_section,
			.StorageClass = IMAGE_SYM_CLASS_STATIC,
			.NumberOfAuxSymbols = 1,
		});
		utf8 rodata_section_size[8];
		*(u64 *)rodata_section_size = rodata_section.data.count;
		writer.symbols.add({
			.name = rodata_section_size,
		});

		writer.symbols.add({
			.name = ".absolut"str,
			.section = coff::Writer::absolute_section,
			.StorageClass = IMAGE_SYM_CLASS_STATIC,
		});

		writer.symbols.add({
			.name = "main"str,
			.section = &text_section,
			.StorageClass = IMAGE_SYM_CLASS_EXTERNAL,
		});

		for_each(external_relocations, [&](auto name, auto relocations) {
			auto symbol = &writer.symbols.add({
				.name = name,
				.section = coff::Writer::undefined_section,
				.StorageClass = IMAGE_SYM_CLASS_EXTERNAL,
			});
			for (auto relocation : relocations) {
				relocation->symbol = symbol;
			}
		});

		for (auto &relocation : rodata_relocations) {
			relocation->symbol = &rodata_symbol;
		}
		for (auto &relocation : text_relocations) {
			relocation->symbol = &text_symbol;
			auto ptr = (u64 *)(buf + relocation->offset);
			*ptr = instruction_offsets.find(*ptr)->value;
		}

		write(writer, obj_path);


		{
			print("==== test.obj ====\n");
			print(coff::read("test.obj"s).value());
		}
		{
			print("==== a.obj ====\n");
			print(coff::read("a.obj"s).value());
		}
	}

		return false;

	{
		scoped_phase("Assembling and linking");

		StringBuilder bat_builder;

		append_format(bat_builder, "@echo off\n");
		append_format(bat_builder, R"("{}link" /nologo "{}.obj" /out:"{}" /nodefaultlib /entry:"main" /subsystem:console /DEBUG:FULL /LIBPATH:"{}" kernel32.lib)",
			msvc_directory,
			output_path_base,
			compiler->output_path,
			wkits_directory
		);
		for_each(compiler->extern_libraries, [&](auto library, auto) {
			append_format(bat_builder, " {}.lib", library);
		});

		auto bat_path = u8"tlang_build.bat"s;
		write_entire_file(bat_path, as_bytes(to_string(bat_builder)));
#if 1
		timed_block(compiler->profiler, "link"s);

		auto process = start_process(bat_path);
		if (!process.handle) {
			with(ConsoleColor::red, print("Cannot execute file '{}'\n", bat_path));
			return false;
		}

		defer { free(process); };

		bat_builder.clear();
		auto &compile_log = bat_builder;

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
			with(ConsoleColor::red, print("Build command failed\n"));
			return false;
		}
#endif
		if (!compiler->keep_temp)
			delete_file(bat_path);
	}

	if (!compiler->keep_temp)
		delete_file(obj_path);

	return true;
}

DECLARE_TARGET_INFORMATION_GETTER {
	::compiler = compiler;
	compiler->stack_word_size = 8;
	compiler->register_size = 8;
	compiler->general_purpose_register_count = 16;
}
