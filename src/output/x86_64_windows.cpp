#define TL_IMPL
#pragma warning(disable: 4702) // unreachable
#include <bytecode.h>
#include <ast.h>
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
auto fe(::Address r) {
	auto scale = lea_scales[r.r1_scale_index];
	return FE_MEM(fe(r.base), scale, scale ? fe(r.r1) : 0, r.c);
}

DECLARE_OUTPUT_BUILDER {
	init_allocator();
	current_printer = console_printer;

	timed_function(context.profiler);

	auto output_path_base = format("{}\\{}", context.current_directory, parse_path(context.source_path).name);
	auto obj_path = to_pathchars(format(u8"{}.obj", output_path_base));

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
			.name = parse_path(context.source_path).name_and_extension(),
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

		// This struct is just for local function overloading.....
		struct Opcoder {
			StringBuilder &text_builder;
			coff::Writer::Section &text_section;
			Bytecode &bytecode;
			HashMap<String, List<coff::Writer::Relocation *>> relocations;

			void w1(u8  v) { append(text_builder, value_as_bytes(v)); }
			void w2(u16 v) { append(text_builder, value_as_bytes(v)); }
			void w4(u32 v) { append(text_builder, value_as_bytes(v)); }
			void w8(u64 v) { append(text_builder, value_as_bytes(v)); }

			void w1(ModRM v) { append(text_builder, value_as_bytes(v)); }

			inline constexpr bool is_sign_extendable(s64 s) {
				return (s&0xffff'ffff'8000'0000) == 0xffff'ffff'8000'0000 || (s&0xffff'ffff'8000'0000) == 0;
			}

			u8 rid(Register64 r) {
				assert((u8)r < 8);
				return (u8)r;
			}
			u8 ridw(Register64 r) {
				return (u8)r & 7;
			}
			void mov(Register64 r, u32 i) {
				u8 b[5];
				b[0] = 0xb8 + rid(r);
				b[1] = (i >>  0) & 0xff;
				b[2] = (i >>  8) & 0xff;
				b[3] = (i >> 16) & 0xff;
				b[4] = (i >> 24) & 0xff;
				append(text_builder, array_as_span(b));
			}
			void call(u32 i) {
				u8 b[5];
				b[0] = 0xe8;
				b[1] = (i >>  0) & 0xff;
				b[2] = (i >>  8) & 0xff;
				b[3] = (i >> 16) & 0xff;
				b[4] = (i >> 24) & 0xff;
				append(text_builder, array_as_span(b));
			}
			void push_c(s64 i) {
				if (is_sign_extendable(i)) {
					w1(0x68);
					w4(i);
				} else {
					sub_rc(rsp, 8);
					mov4_mc(rsp, i);
					mov4_mc(rsp+4, i>>32);
				}
			}
			void push(Register64 r) {
				if ((u8)r >= 8)
					append(text_builder, value_as_bytes(rex_b));
				append(text_builder, value_as_bytes((u8)(0x50 + ridw(r))));
			}
			void pop(Register64 r) {
				if ((u8)r >= 8)
					append(text_builder, value_as_bytes(rex_b));
				append(text_builder, value_as_bytes((u8)(0x58 + ridw(r))));
			}
			void _and(Register64 r, s8 i) {
				append(text_builder, value_as_bytes(rex_w));
				append(text_builder, value_as_bytes((u8)0x83));
				ModRM m = {};
				m.mod = 3;
				m.reg = 4;
				m.rm = rid(r);
				append(text_builder, value_as_bytes(m));
				append(text_builder, value_as_bytes(i));
			}
			void cld() {
				append(text_builder, value_as_bytes((u8)0xfc));
			}
			void add_rc(Register64 d, s64 s) {
				assert((s&0xffff'ffff'8000'0000) == 0xffff'ffff'8000'0000 || (s&0xffff'ffff'8000'0000) == 0);

				u8 prefix = rex_w;
				if (is_gpr(d)) prefix |= rex_b;
				w1(prefix);
				w1(0x81);
				w1(0xc0 + ridw(d));
				w4(s);
			}
			void add_mc(xAddress d, s64 s) {
				assert((s&0xffff'ffff'8000'0000) == 0xffff'ffff'8000'0000 || (s&0xffff'ffff'8000'0000) == 0);

				u8 prefix = rex_w;
				if (is_gpr(d.base)) prefix |= rex_b;
				w1(prefix);
				w1(0x81);
				w1(0x00 + ridw(d.base));
				w4(s);
			}
			void sub_rc(Register64 d, s64 s) {
				assert((s&0xffff'ffff'8000'0000) == 0xffff'ffff'8000'0000 || (s&0xffff'ffff'8000'0000) == 0);

				u8 prefix = rex_w;
				if (is_gpr(d)) prefix |= rex_b;
				w1(prefix);
				w1(0x81);
				w1(0xe8 + ridw(d));
				w4(s);
			}
			void mov_rc(Register64 d, s64 s) {
				auto write_prefix = [&] {
					u8 prefix = rex_w;
					if (is_gpr(d)) prefix |= rex_b;
					w1(prefix);
				};

				if ((s & 0xffff'ffff'8000'0000) == 0xffff'ffff'8000'0000) {
					// sign extendable
					write_prefix();
					w1(0xc7);
					w1(0xc0 + ridw(d));
					w4(s);
				} else if ((s & 0xffff'ffff'0000'0000) == 0) {
					// zero extendable
					if (is_gpr(d)) {
						w1(0x41); // prefix
					}
					w1(0xb8 + ridw(d));
					w4(s);
				} else {
					write_prefix();
					w1(0xb8 + ridw(d));
					w8(s);
				}
			}
			void mov4_mc(xAddress d, s32 s) {
				assert(d.r1_scale_index == 0);
				assert(d.r2_scale == 0);

				if (is_gpr(d.base)) {
					w1(rex_b);
				}

				w1(0xc7);

				w1(0x80+ridw(d.base));

				if (ridw(d.base) == 4)
					w1(0x24);

				w4(d.c);
				w4(s);
			}
			void mov8_mr(xAddress d, Register64 s) {
				assert(d.r1_scale_index == 0);
				assert(d.r2_scale == 0);

				u8 prefix = rex_w;
				if (is_gpr(d.base)) prefix |= rex_b;
				if (is_gpr(s))      prefix |= rex_r;
				w1(prefix);

				w1(0x89);

				ModRM m = {};
				if (d.c || ridw(d.base) == 5)
					m.mod |= 1;
				m.reg = ridw(s);
				m.rm  = ridw(d.base);
				w1(m);

				if (ridw(d.base) == 4)
					append(text_builder, value_as_bytes((u8)0x24));

				if (d.c || ridw(d.base) == 5)
					append(text_builder, value_as_bytes((u8)d.c));
			}
			void mov8_rm(Register64 d, xAddress s) {
				assert(s.r1_scale_index == 0);
				assert(s.r2_scale == 0);

				u8 prefix = rex_w;
				if (is_gpr(s.base)) prefix |= rex_b;
				if (is_gpr(d))      prefix |= rex_r;
				w1(prefix);

				w1(0x8b);

				ModRM m = {};
				if (s.c || ridw(s.base) == 5)
					m.mod |= 1;
				m.reg = ridw(d);
				m.rm  = ridw(s.base);
				w1(m);

				if (ridw(s.base) == 4)
					append(text_builder, value_as_bytes((u8)0x24));

				if (s.c || ridw(s.base) == 5)
					append(text_builder, value_as_bytes((u8)s.c));
			}
			void lea(Register64 d, xAddress s) {
				// TODO: use versions with smaller immediates
				// REX.W + 8D /r		LEA r64,m		Store effective address for m in register r64.
				u8 prefix = rex_w;
				if (is_gpr(s.base)) prefix |= rex_b;
				if (is_gpr(d     )) prefix |= rex_r;
				append(text_builder, value_as_bytes(prefix));

				append(text_builder, value_as_bytes((u8)0x8d));

				assert(s.r1_scale_index == 0);
				assert(s.r2_scale == 0);

				ModRM m = {};
				m.mod = 2;
				m.rm  = ridw(s.base);
				m.reg = ridw(d);
				append(text_builder, value_as_bytes(m));

				append(text_builder, value_as_bytes(s.c));
			}
			void rep_stosb() {
				w1(0xf3);
				w1(0xaa);
			}
			void rep_movsb() {
				w1(0xf3);
				w1(0xa4);
			}
			void xchg8_m(xAddress a, Register64 b) {
				if (a.c == 0) {
					u8 prefix = rex_w;
					if (is_gpr(a.base)) prefix |= rex_b;
					if (is_gpr(b))      prefix |= rex_r;
					w1(prefix);

					w1(0x87);

					w1(ridw(a.base) | (ridw(b) << 3));
				} else {
					not_implemented();
				}
			}
			void convert() {
				xchg8_m(rax, rax);
				xchg8_m(rcx, rax);
				xchg8_m(rdx, rax);
				xchg8_m(rbx, rax);
				xchg8_m(rsp, rax);
				xchg8_m(rbp, rax);
				xchg8_m(rsi, rax);
				xchg8_m(rdi, rax);
				xchg8_m(r8 , rax);
				xchg8_m(r9 , rax);
				xchg8_m(r10, rax);
				xchg8_m(r11, rax);
				xchg8_m(r12, rax);
				xchg8_m(r13, rax);
				xchg8_m(r14, rax);
				xchg8_m(r15, rax);

				_and(rsp, -16);
				push_c((s8)0);
				push_c((s8)0);
				cld();
				call((u32)10);
				u32 *main_call = (u32 *)text_builder.last->end() - 1;

				mov8_rm(rcx, rsp);


				call((u32)0);
				relocations.get_or_insert("ExitProcess"str).add(&text_section.relocations.add({.offset=(u32)text_builder.count()-4,.type=IMAGE_REL_AMD64_REL32}));


				for (auto i : bytecode.instructions) {
					switch (i.kind) {
						using enum InstructionKind;
						case jmp_label: break;
						case push_used_registers: {
							REDECLARE_REF(i, i.push_used_registers);
							for (u64 bit = 0; bit < sizeof(i.mask) * 8; ++bit) {
								if ((i.mask >> bit) & 1) {
									push(to_x86_register((Register)bit));
								}
							}
							// keep the stack 16-byte aligned
							if (count_bits(i.mask) & 1)
								this->sub_rc(rsp, 8);
							break;
						}
						case pop_used_registers: {
							REDECLARE_REF(i, i.pop_used_registers);
							// keep the stack 16-byte aligned
							if (count_bits(i.mask) & 1)
								this->add_rc(rsp, 8);
							for (u64 bit = 0; bit < sizeof(i.mask) * 8; ++bit) {
								if ((i.mask >> bit) & 1) {
									pop(to_x86_register((Register)bit));
								}
							}
							break;
						}
						case push_c: {
							REDECLARE_REF(i, i.push_c);
							this->push_c((s32)i.s);
							break;
						}
						case push_r: {
							REDECLARE_REF(i, i.push_r);
							push(to_x86_register(i.s));
							break;
						}
						case pop_r: {
							REDECLARE_REF(i, i.pop_r);
							pop(to_x86_register(i.d));
							break;
						}
						case mov_rr: {
							REDECLARE_REF(i, i.mov_rr);
							// REX.W + 89 /r		MOV r/m64,r64		Move r64 to r/m64.
							append(text_builder, value_as_bytes(rex_w));
							append(text_builder, value_as_bytes((u8)0x89));
							ModRM m = {};
							m.mod = 3;
							m.reg = rid(to_x86_register(i.s));
							m.rm  = rid(to_x86_register(i.d));
							append(text_builder, value_as_bytes(m));
							break;
						}
						case mov_re: {
							REDECLARE_REF(i, i.mov_re);
							auto d = to_x86_register(i.d);
							u8 prefix = rex_w;
							if (is_gpr(d)) prefix |= rex_b;
							w1(prefix);
							w1(0xb8 + ridw(d));
							w8(0);
							relocations.get_or_insert(i.s).add(&text_section.relocations.add({.offset=(u32)text_builder.count()-8,.type=IMAGE_REL_AMD64_ADDR64}));
							break;
						}
						case mov8_mr: {
							REDECLARE_REF(i, i.mov8_mr);
							this->mov8_mr(i.d, to_x86_register(i.s));
							break;
						}
						case mov8_rm: {
							REDECLARE_REF(i, i.mov8_rm);
							this->mov8_rm(to_x86_register(i.d), i.s);
							break;
						}
						case lea: {
							REDECLARE_REF(i, i.lea);
							this->lea(to_x86_register(i.d), i.s);
							break;
						}
						case ret: {
							w1(0xc3);
							break;
						}
						case add_rc: {
							REDECLARE_REF(i, i.add_rc);
							this->add_rc(to_x86_register(i.d), i.s);
							break;
						}
						case add_mc: {
							REDECLARE_REF(i, i.add_mc);
							this->add_mc(i.d, i.s);
							break;
						}
						case sub_rc: {
							REDECLARE_REF(i, i.sub_rc);
							this->add_rc(to_x86_register(i.d), i.s);
							break;
						}
						case call_r: {
							REDECLARE_REF(i, i.call_r);
							auto s = to_x86_register(i.s);
							if (is_gpr(s))
								w1(rex_b);
							w1(0xff);
							w1(0xd0 + ridw(s));
							break;
						}
						case setf_mcc: {
							REDECLARE_REF(i, i.setf_mcc);
							this->lea(rdi, i.d);
							this->mov_rc(rax, i.s);
							this->mov_rc(rcx, i.size);
							rep_stosb();
							break;
						}
						case copyf_mmc: {
							REDECLARE_REF(i, i.copyf_mmc);
							this->lea(rsi, i.s);
							this->lea(rdi, i.d);
							this->mov_rc(rcx, i.size);
							rep_movsb();
							break;
						}
						case noop:
							break;
						case xchg8_m: {
							REDECLARE_REF(i, i.xchg8_m);

							auto a = to_x86_register(i.a.base);
							auto b = to_x86_register(i.b);


							break;
						}
						default:
							invalid_code_path();
					}
				}
			}
		};
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
		local_relocations.get_or_insert(cur-4) = index_of(bytecode.instructions, context.main_lambda->first_instruction);
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
				case push_used_registers: {
					REDECLARE_REF(i, i.push_used_registers);
					if (i.mask == -1) {
						e(FE_PUSHr, FE_BX);
						e(FE_PUSHr, FE_SI);
						e(FE_PUSHr, FE_DI);
						e(FE_PUSHr, FE_R12);
						e(FE_PUSHr, FE_R13);
						e(FE_PUSHr, FE_R14);
						e(FE_PUSHr, FE_R15);
						e(FE_SUB64ri, FE_SP, 8);
						break;
					}
					for (u64 bit = 0; bit < sizeof(i.mask) * 8; ++bit) {
						if ((i.mask >> bit) & 1) {
							e(FE_PUSHr, fe((Register)bit));
						}
					}
					// keep the stack 16-byte aligned
					if (count_bits(i.mask) & 1)
						e(FE_SUB64ri, FE_SP, 8);
					break;
				}
				case pop_used_registers: {
					REDECLARE_REF(i, i.pop_used_registers);
					if (i.mask == -1) {
						e(FE_ADD64ri, FE_SP, 8);
						e(FE_POPr, FE_R15);
						e(FE_POPr, FE_R14);
						e(FE_POPr, FE_R13);
						e(FE_POPr, FE_R12);
						e(FE_POPr, FE_DI);
						e(FE_POPr, FE_SI);
						e(FE_POPr, FE_BX);
						break;
					}
					// keep the stack 16-byte aligned
					if (count_bits(i.mask) & 1)
						e(FE_ADD64ri, FE_SP, 8);
					for (u64 bit = 0; bit < sizeof(i.mask) * 8; ++bit) {
						if ((i.mask >> bit) & 1) {
							e(FE_POPr, fe((Register)bit));
						}
					}
					break;
				}
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
				case push_a:
					e(FE_MOV64ri, FE_AX, 0xaaaaaaaaaaaaaaaa);
					((u64 *)cur)[-1] = i.push_a.s;
					rodata_relocations.add(&text_section.relocations.add({.offset=size()-8,.type=IMAGE_REL_AMD64_ADDR64}));

					e(FE_PUSHr, FE_AX);
					break;
				case pop_r: e(FE_POPr, fe(i.pop_r.d)); break;

				case mov_rc: e(FE_MOV64ri, fe(i.mov_rc.d), i.mov_rc.s); break;
				case mov_rr: e(FE_MOV64rr, fe(i.mov_rr.d), fe(i.mov_rr.s)); break;
				case mov_re:
					e(FE_MOV64ri, fe(i.mov_re.d), 0xaaaaaaaaaaaaaaaa);
					((u64 *)cur)[-1] = 0;
					external_relocations.get_or_insert(i.mov_re.s).add(&text_section.relocations.add({.offset=size()-8,.type=IMAGE_REL_AMD64_ADDR64}));
					break;
				case mov_rt:
					e(FE_MOV64ri, fe(i.mov_rt.d), 0xaaaaaaaaaaaaaaaa);
					((u64 *)cur)[-1] = i.mov_rt.s;
					text_relocations.add(&text_section.relocations.add({.offset=size()-8,.type=IMAGE_REL_AMD64_ADDR64}));
					break;

				case mov1_rm: e(FE_MOV8rm , fe(i.mov1_rm.d), fe(i.mov1_rm.s)); break;
				case mov2_rm: e(FE_MOV16rm, fe(i.mov2_rm.d), fe(i.mov2_rm.s)); break;
				case mov4_rm: e(FE_MOV32rm, fe(i.mov4_rm.d), fe(i.mov4_rm.s)); break;
				case mov8_rm: e(FE_MOV64rm, fe(i.mov8_rm.d), fe(i.mov8_rm.s)); break;

				case mov1_mr: e(FE_MOV8mr , fe(i.mov1_mr.d), fe(i.mov1_mr.s)); break;
				case mov2_mr: e(FE_MOV16mr, fe(i.mov2_mr.d), fe(i.mov2_mr.s)); break;
				case mov4_mr: e(FE_MOV32mr, fe(i.mov4_mr.d), fe(i.mov4_mr.s)); break;
				case mov8_mr: e(FE_MOV64mr, fe(i.mov8_mr.d), fe(i.mov8_mr.s)); break;

				case xchg8_m: e(FE_XCHG64mr, fe(i.xchg8_m.a), fe(i.xchg8_m.b)); break;

				case lea: e(FE_LEA64rm, fe(i.lea.d), fe(i.lea.s)); break;

				case negi_r: e(FE_NEG64r, fe(i.negi_r.d)); break;

				case sub_rr: e(FE_SUB64rr, fe(i.sub_rr.d), fe(i.sub_rr.s)); break;
				case sub_rc: e(FE_SUB64ri, fe(i.sub_rc.d), i.sub_rc.s); break;
				case sub_mc: e(FE_SUB64mi, fe(i.sub_mc.d), i.sub_mc.s); break;
				case sub_mr: e(FE_SUB64mr, fe(i.sub_mr.d), fe(i.sub_mr.s)); break;

				case add_rr: e(FE_ADD64rr, fe(i.add_rr.d), fe(i.add_rr.s)); break;
				case add_rc: e(FE_ADD64ri, fe(i.add_rc.d), i.add_rc.s); break;
				case add_mc: e(FE_ADD64mi, fe(i.add_mc.d), i.add_mc.s); break;
				case add_mr: e(FE_ADD64mr, fe(i.add_mr.d), fe(i.add_mr.s)); break;

				case and_mc: e(FE_AND64mi, fe(i.and_mc.d), i.and_mc.s); break;
				case and_mr: e(FE_AND64mr, fe(i.and_mr.d), fe(i.and_mr.s)); break;

				case or_mc: e(FE_OR64mi, fe(i.or_mc.d), i.or_mc.s); break;
				case or_mr: e(FE_OR64mr, fe(i.or_mr.d), fe(i.or_mr.s)); break;

				case xor_mc: e(FE_XOR64mi, fe(i.xor_mc.d), i.xor_mc.s); break;
				case xor_mr: e(FE_XOR64mr, fe(i.xor_mr.d), fe(i.xor_mr.s)); break;

				case shl_mr:
					e(FE_MOV8rr, FE_CX, fe(i.shl_mr.s));
					e(FE_SHL64mr, fe(i.shl_mr.d), FE_CX);
					break;
				case shr_mr:
					e(FE_MOV8rr, FE_CX, fe(i.shr_mr.s));
					e(FE_SHR64mr, fe(i.shr_mr.d), FE_CX);
					break;

				case div_mr:
					if (to_x86_register(i.div_mr.s) == rdx) {
						e(FE_MOV64rr, FE_BX, FE_DX);
						e(FE_XOR64rr, FE_DX, FE_DX);
						e(FE_MOV64rm, FE_AX, fe(i.div_mr.d));
						e(FE_DIV64r, FE_BX);
						e(FE_MOV64mr, fe(i.div_mr.d), FE_AX);
						e(FE_MOV64rr, FE_DX, FE_BX);
					} else {
						e(FE_MOV64rr, FE_BX, FE_DX);
						e(FE_XOR64rr, FE_DX, FE_DX);
						e(FE_MOV64rm, FE_AX, fe(i.div_mr.d));
						e(FE_DIV64r, fe(i.div_mr.s));
						e(FE_MOV64mr, fe(i.div_mr.d), FE_AX);
						e(FE_MOV64rr, FE_DX, FE_BX);
					}
					break;
				case mod_mr:
					if (to_x86_register(i.div_mr.s) == rdx) {
						e(FE_MOV64rr, FE_BX, FE_DX);
						e(FE_XOR64rr, FE_DX, FE_DX);
						e(FE_MOV64rm, FE_AX, fe(i.div_mr.d));
						e(FE_DIV64r, FE_BX);
						e(FE_MOV64mr, fe(i.div_mr.d), FE_DX);
						e(FE_MOV64rr, FE_DX, FE_BX);
					} else {
						e(FE_MOV64rr, FE_BX, FE_DX);
						e(FE_XOR64rr, FE_DX, FE_DX);
						e(FE_MOV64rm, FE_AX, fe(i.div_mr.d));
						e(FE_DIV64r, fe(i.div_mr.s));
						e(FE_MOV64mr, fe(i.div_mr.d), FE_DX);
						e(FE_MOV64rr, FE_DX, FE_BX);
					}
					break;

				case setf_mcc: {
					REDECLARE_REF(i, i.setf_mcc);
					e(FE_LEA64rm, FE_DI, fe(i.d));
					e(FE_MOV64ri, FE_AX, i.s);
					e(FE_MOV64ri, FE_CX, i.size);
					e(FE_REP_STOS8);
					break;
				}
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
			print(coff::read("test.obj").value());
		}
		{
			print("==== a.obj ====\n");
			print(coff::read("a.obj").value());
		}
	}

		return;

	{
		scoped_phase("Assembling and linking");

		StringBuilder bat_builder;

		append_format(bat_builder, "@echo off\n");
		append_format(bat_builder, R"("{}link" /nologo "{}.obj" /out:"{}" /nodefaultlib /entry:"main" /subsystem:console /DEBUG:FULL /LIBPATH:"{}" kernel32.lib)",
			msvc_directory,
			output_path_base,
			context.output_path,
			wkits_directory
		);
		for_each(bytecode.extern_libraries, [&](auto library, auto) {
			append_format(bat_builder, " {}.lib", library);
		});

		auto bat_path = u8"tlang_build.bat"s;
		write_entire_file(bat_path, as_bytes(to_string(bat_builder)));
#if 1
		timed_block(context.profiler, "link"s);

		auto process = start_process(bat_path);
		if (!process.handle) {
			print(Print_error, "Cannot execute file '{}'\n", bat_path);
			return;
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
			print(Print_error, "Build command failed\n");
			return;
		}
#endif
		if (!context.keep_temp)
			delete_file(bat_path);
		print("Build succeeded\n");
	}

	if (!context.keep_temp)
		delete_file(obj_path);
}

DECLARE_TARGET_INFORMATION_GETTER {
	context.stack_word_size = 8;
	context.register_size = 8;
	context.general_purpose_register_count = 16;
}
