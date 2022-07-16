#pragma once
#include <bytecode.h>
#include <ast.h>
#include <tl/hash_set.h>

#define NOMINMAX
#include <Windows.h>

using enum Register;

inline umm append(StringBuilder &builder, Register r) {
	switch (r) {
		case rs: return append(builder, "rs");
		case rb: return append(builder, "rb");
		case parameters       : return append(builder, "parameters");
		case return_parameters: return append(builder, "return_parameters");
		case locals           : return append(builder, "locals");
		case temporary        : return append(builder, "temporary");
		case constants        : return append(builder, "constants");
		case rwdata           : return append(builder, "rwdata");
		case zeros            : return append(builder, "zeros");
		case instructions     : return append(builder, "instructions");
	}
	return append_format(builder, "r{}", (u8)r);
}
inline umm append(StringBuilder &builder, XRegister r) {
	using enum XRegister;
	switch (r) {
		case x0: return append(builder, "x0");
		case x1: return append(builder, "x1");
		case x2: return append(builder, "x2");
		case x3: return append(builder, "x3");
	}
	invalid_code_path();
	return {};
}
inline umm append(StringBuilder &builder, Address a) {
	umm result = 0;
	result += append(builder, '[');
	result += append(builder, a.base);
	if (a.r1_scale_index) {
		if (a.r2_scale) {
			invalid_code_path("not implemented");
		} else {
			result += append(builder, '+');
			result += append(builder, a.r1);
			result += append(builder, '*');
			result += append(builder, lea_scales[a.r1_scale_index]);
			if (a.c) {
				result += append(builder, '+');
				result += append(builder, a.c);
			}
		}
	} else {
		if (a.r2_scale) {
			invalid_code_path("not implemented");
		} else {
			if (a.c) {
				if (a.c >= 0) {
					result += append(builder, '+');
					result += append(builder, a.c);
				} else {
					result += append(builder, '-');
					result += append(builder, -a.c);
				}
			}
		}
	}
	result += append(builder, ']');
	return result;
}

inline void run_bytecode(Compiler &compiler, Span<Instruction> _instructions_, AstLambda *main_lambda, ExternLibraries extern_libraries) {
	assert(main_lambda->location_in_bytecode != -1);

	timed_function(compiler.profiler);

	// NOTE: instructions need to be modified to efficiently run the bytecode, and the caller might not expect changed instructions.
	auto _instructions = to_list(_instructions_);

	u64 registers[256] {};

	u8 _constants[1024*1024] = {};
	u8 _rwdata   [1024*1024] = {};
	u8 _zeros    [1024*1024] = {};

	auto append_section = [&](Section &section, u8 *buffer) {
		auto it = section.buffer.begin();
		umm i = 0;
		bool last_is_byte = true;

		while (it != section.buffer.end()) {
			auto relocation = binary_search(section.relocations, i);
			if (relocation) {
				u64 offset = 0;
				for (umm j = 0; j < 8; ++j)
					offset = (offset >> 8) | ((u64)*it++ << 56);

				*(u64 *)&buffer[i] = (u64)(buffer + offset);
				i += 8;
			} else {
				buffer[i++] = *it++;
			}
		}
	};

	append_section(compiler.constant_section, _constants);
	append_section(compiler.data_section, _rwdata);

	struct CallFrame {
		u8 locals   [1024*1024];
		u8 temporary[1024*1024];
	};

	CallFrame *call_frames[256] = {};
	umm top_call_frame_index = -1;


#if 1
	// register
#define R(r) (registers[(u8)(r)])

	// memory
#define M(a) ((u8 *)R((a).base) + R((a).r1) * lea_scales.data[(a).r1_scale_index] + (a).c)

	// sized memory
#define M1(a) ((u8  *)M(a))
#define M2(a) ((u16 *)M(a))
#define M4(a) ((u32 *)M(a))
#define M8(a) ((u64 *)M(a))

	// stack
#define PUSH(v) (R(rs) -= 8, *M8(Address(rs)) = (v))
#define POP()   (R(rs) += 8, *M8(Address(rs) - 8))

#else
	auto r = [&](Register r) -> u64 & {
		return registers[(u8)r];
	};

	auto m = [&](Address a) {
		auto base = (u8 *)r(a.base);
		auto r1 = r(a.r1);
		return base + r1 * lea_scales.data[a.r1_scale_index] + a.c;
	};

	auto m1 = [&](Address a) { return (u8  *)m(a); };
	auto m2 = [&](Address a) { return (u16 *)m(a); };
	auto m4 = [&](Address a) { return (u32 *)m(a); };
	auto m8 = [&](Address a) { return (u64 *)m(a); };


#define READ1(d, s) do { auto &ref = r(d); auto addr = m1(s); dprint("from {}", addr); ref = *addr; dprint(" read {}\n", ref); } while (0)
#define READ2(d, s) do { auto &ref = r(d); auto addr = m2(s); dprint("from {}", addr); ref = *addr; dprint(" read {}\n", ref); } while (0)
#define READ4(d, s) do { auto &ref = r(d); auto addr = m4(s); dprint("from {}", addr); ref = *addr; dprint(" read {}\n", ref); } while (0)
#define READ8(d, s) do { auto &ref = r(d); auto addr = m8(s); dprint("from {}", addr); ref = *addr; dprint(" read {}\n", ref); } while (0)

	auto write1c = [&](Address d, u64 s) { dprint("to {} ", m(d)); *m1(d) = s; dprint("wrote {}\n", s); };
	auto write2c = [&](Address d, u64 s) { dprint("to {} ", m(d)); *m2(d) = s; dprint("wrote {}\n", s); };
	auto write4c = [&](Address d, u64 s) { dprint("to {} ", m(d)); *m4(d) = s; dprint("wrote {}\n", s); };
	auto write8c = [&](Address d, u64 s) { dprint("to {} ", m(d)); *m8(d) = s; dprint("wrote {}\n", s); };

	auto write1 = [&](Address d, Register s) { dprint("to {} ", m(d)); auto &ref = r(s); *m1(d) = ref; dprint("wrote {}\n", ref); };
	auto write2 = [&](Address d, Register s) { dprint("to {} ", m(d)); auto &ref = r(s); *m2(d) = ref; dprint("wrote {}\n", ref); };
	auto write4 = [&](Address d, Register s) { dprint("to {} ", m(d)); auto &ref = r(s); *m4(d) = ref; dprint("wrote {}\n", ref); };
	auto write8 = [&](Address d, Register s) { dprint("to {} ", m(d)); auto &ref = r(s); *m8(d) = ref; dprint("wrote {}\n", ref); };

	auto push = [&](u64 v) {
		R(rs) -= 8;
		*m8(Address(rs)) = v;
	};

	auto pop = [&]() {
		defer { R(rs) += 8; };
		return *m8(Address(rs));
	};

#endif

	u8 stack [1024*1024] = {};

	R(instructions) = (u64)_instructions.data;
	R(constants) = (u64)array_as_span(_constants).begin();
	R(rwdata   ) = (u64)array_as_span(_rwdata   ).begin();
	R(zeros    ) = (u64)array_as_span(_zeros    ).begin();
	R(rb       ) = (u64)array_as_span(stack    ).end();
	R(rs       ) = (u64)array_as_span(stack    ).end();

	// Patch parameters and return parameters
	{
		u64 parameters_size = 0;
		for (auto &i : _instructions) {
			if (i.kind == InstructionKind::begin_lambda) {
				parameters_size = i.begin_lambda.lambda->parameters_size;
			}
			for (auto offset : address_members_offsets[(int)i.kind]) {
				auto &a = *(Address *)((u8 *)&i + offset);
				switch (a.base) {
					case parameters:
						a.base = rb;
						a.c = parameters_size - a.c + 8;
						break;
					case return_parameters:
						a.base = rb;
						a.c += parameters_size + 16;
						break;
					case instructions:
						a.c *= sizeof(Instruction);
						break;
				}
			}
		}
	}

	Instruction *rip = _instructions.data + main_lambda->location_in_bytecode;

	HashMap<String, HMODULE> modules;
	HashMap<String, HMODULE *> fn_to_lib;

	for_each(extern_libraries, [&](String lib, List<String> funcs) {
		for (auto func : funcs) {
			auto &module = modules.get_or_insert(lib);
			if (!module) {
				module = LoadLibraryA((char *)null_terminate(lib).data);
			}
			fn_to_lib.get_or_insert(func) = &module;
		}
	});

	for (u32 i = 0; i < 256; ++i) {

//        print(R"(<Item Name="[value]" Condition="(unsigned char)((int)kind+instruction_counter_base)=={}" Optional="true">__v{}</Item>
//)", i, i);
//		print(R"(<DisplayString Condition="(unsigned char)((int)kind+instruction_counter_base)=={}" Optional="true">{{__v{}}}</DisplayString>
//)", i, i);
	}

#define dprint(...)
// #define dprint print

	Comparison cmp_flag = {};

	u64 total_instructions_executed = 0;

	auto timer = create_precise_timer();

	while (1) {
		defer { total_instructions_executed++; };

		auto &i = *rip;

#if BYTECODE_DEBUG
		if (i.comment.data)
			with(ConsoleColor::dark_cyan, dprint("{}\n", i.comment));
		//with(ConsoleColor::gray, print_instruction(i));
		dprint("\n");

		//if (find(i.comment, u8"xxxyyy"s))
		//	debug_break();
#endif

		switch (i.kind) {
			using enum InstructionKind;
			case mov_rc: R(i.mov_rc.d) = i.mov_rc.s; break;
			case mov_rr: R(i.mov_rr.d) = R(i.mov_rr.s); break;
			case mov1_rm: R(i.mov1_rm.d) = *M1(i.mov1_rm.s); break;
			case mov2_rm: R(i.mov2_rm.d) = *M2(i.mov2_rm.s); break;
			case mov4_rm: R(i.mov4_rm.d) = *M4(i.mov4_rm.s); break;
			case mov8_rm: R(i.mov8_rm.d) = *M8(i.mov8_rm.s); break;
			case mov1_mc: *M1(i.mov1_mc.d) = i.mov1_mc.s; break;
			case mov2_mc: *M2(i.mov2_mc.d) = i.mov2_mc.s; break;
			case mov4_mc: *M4(i.mov4_mc.d) = i.mov4_mc.s; break;
			case mov8_mc: *M8(i.mov8_mc.d) = i.mov8_mc.s; break;
			case mov1_mr: *M1(i.mov1_mr.d) = R(i.mov1_mr.s); break;
			case mov2_mr: *M2(i.mov2_mr.d) = R(i.mov2_mr.s); break;
			case mov4_mr: *M4(i.mov4_mr.d) = R(i.mov4_mr.s); break;
			case mov8_mr: *M8(i.mov8_mr.d) = R(i.mov8_mr.s); break;
			case begin_lambda: {
				auto lambda = i.begin_lambda.lambda;

				auto &frame = call_frames[++top_call_frame_index];
				if (!frame) {
					frame = page_allocator.allocate<CallFrame>();
				}

				R(locals)    = (u64)array_as_span(frame->locals).begin();
				R(temporary) = (u64)array_as_span(frame->temporary).begin();

				PUSH(R(rb));
				R(rb) = R(rs);

				for_each(lambda->used_registers, [&](umm bit) {
					PUSH(registers[bit]);
				});

				// keep the stack 16-byte aligned
				if (lambda->used_registers.count() & 1) {
					R(rs) -= 8;
				}


				auto used_bytes = lambda->max_stack_space_used_for_call;

				if (used_bytes) {
					R(rs) -= used_bytes;
				}
				break;
			}
			case end_lambda: {
				auto lambda = i.end_lambda.lambda;

				top_call_frame_index--;
				if (top_call_frame_index != -1) {
					R(locals)    = (u64)array_as_span(call_frames[top_call_frame_index]->locals).begin();
					R(temporary) = (u64)array_as_span(call_frames[top_call_frame_index]->temporary).begin();
				}

				auto used_bytes = lambda->max_stack_space_used_for_call;

				// keep the stack 16-byte aligned
				if (lambda->used_registers.count() & 1) {
					used_bytes += 8;
				}
				if (used_bytes) {
					R(rs) += used_bytes;
				}

				for_each(lambda->used_registers, [&](umm bit) {
					registers[bit] = POP();
				});

				R(rs) = R(rb);
				R(rb) = POP();

				if (lambda == main_lambda) {
					goto halt;
				}

				// NOTE: rip will be incremented at the end of iteration
				rip = (Instruction *)POP();
				break;
			}
			case jmp_label:
			case noop:
				break;
			case mov_re: {
				auto module = fn_to_lib.find(i.mov_re.s);
				assert(module);
				auto addr = GetProcAddress(*module->value, (char *)null_terminate(i.mov_re.s).data);
				assert(addr);
				R(i.mov_re.d) = (u64)addr;
				break;
			}
			case call_r: {
				auto lambda = i.call_r.lambda;
				if (lambda->convention == CallingConvention::stdcall) {
					auto fn = (u64 (__stdcall *)(u64, u64, u64, u64, u64, u64, u64, u64, u64, u64, u64, u64)) R(i.call_r.s);

					auto retval = fn(
						*M8(rs + lambda->parameters_size - 8 * 1),
						*M8(rs + lambda->parameters_size - 8 * 2),
						*M8(rs + lambda->parameters_size - 8 * 3),
						*M8(rs + lambda->parameters_size - 8 * 4),
						*M8(rs + lambda->parameters_size - 8 * 5),
						*M8(rs + lambda->parameters_size - 8 * 6),
						*M8(rs + lambda->parameters_size - 8 * 7),
						*M8(rs + lambda->parameters_size - 8 * 8),
						*M8(rs + lambda->parameters_size - 8 * 9),
						*M8(rs + lambda->parameters_size - 8 * 10),
						*M8(rs + lambda->parameters_size - 8 * 11),
						*M8(rs + lambda->parameters_size - 8 * 12)
					);

					*M8(rs + lambda->parameters_size) = retval;
				} else {
					PUSH((u64)&i);
					auto prev_rip = rip;
					rip = (Instruction *)R(i.call_r.s);
					if (!rip) {
						with(ConsoleColor::red, print("Execution error:\n"));
						print("Attempt to call null.\n");
						compiler.immediate_info(prev_rip->node->location, "Caused from:");
						compiler.immediate_info(lambda->location, "Lambda:");

						// FIXME: probably wanna return an error.
						return;
					}
					continue;
				}

				break;
			}
			case lea: {
				R(i.lea.d) = (u64)M(i.lea.s);
				dprint("loaded {} {} into {}\n", R(i.lea.d), i.lea.s, i.lea.d);
				break;
			}
			case call_c: {
				PUSH((u64)&i);
				rip = _instructions.data + i.call_c.constant;
				continue;
			}
			case cmpu1: {
				REDECLARE_REF(i, i.cmpu1);
				switch (i.c) {
					case Comparison::e:	 R(i.d) = (u8)R(i.a) == (u8)R(i.b); break;
					case Comparison::ne: R(i.d) = (u8)R(i.a) != (u8)R(i.b); break;
					case Comparison::l:	 R(i.d) = (u8)R(i.a) <  (u8)R(i.b); break;
					case Comparison::le: R(i.d) = (u8)R(i.a) <= (u8)R(i.b); break;
					case Comparison::g:	 R(i.d) = (u8)R(i.a) >  (u8)R(i.b); break;
					case Comparison::ge: R(i.d) = (u8)R(i.a) >= (u8)R(i.b); break;
				}
				break;
			}
			case cmpu2: {
				REDECLARE_REF(i, i.cmpu2);
				switch (i.c) {
					case Comparison::e:	 R(i.d) = (u16)R(i.a) == (u16)R(i.b); break;
					case Comparison::ne: R(i.d) = (u16)R(i.a) != (u16)R(i.b); break;
					case Comparison::l:	 R(i.d) = (u16)R(i.a) <  (u16)R(i.b); break;
					case Comparison::le: R(i.d) = (u16)R(i.a) <= (u16)R(i.b); break;
					case Comparison::g:	 R(i.d) = (u16)R(i.a) >  (u16)R(i.b); break;
					case Comparison::ge: R(i.d) = (u16)R(i.a) >= (u16)R(i.b); break;
				}
				break;
			}
			case cmpu4: {
				REDECLARE_REF(i, i.cmpu4);
				switch (i.c) {
					case Comparison::e:	 R(i.d) = (u32)R(i.a) == (u32)R(i.b); break;
					case Comparison::ne: R(i.d) = (u32)R(i.a) != (u32)R(i.b); break;
					case Comparison::l:	 R(i.d) = (u32)R(i.a) <  (u32)R(i.b); break;
					case Comparison::le: R(i.d) = (u32)R(i.a) <= (u32)R(i.b); break;
					case Comparison::g:	 R(i.d) = (u32)R(i.a) >  (u32)R(i.b); break;
					case Comparison::ge: R(i.d) = (u32)R(i.a) >= (u32)R(i.b); break;
				}
				break;
			}
			case cmpu8: {
				REDECLARE_REF(i, i.cmpu8);
				switch (i.c) {
					case Comparison::e:	 R(i.d) = R(i.a) == R(i.b); break;
					case Comparison::ne: R(i.d) = R(i.a) != R(i.b); break;
					case Comparison::l:	 R(i.d) = R(i.a) <  R(i.b); break;
					case Comparison::le: R(i.d) = R(i.a) <= R(i.b); break;
					case Comparison::g:	 R(i.d) = R(i.a) >  R(i.b); break;
					case Comparison::ge: R(i.d) = R(i.a) >= R(i.b); break;
				}
				break;
			}
			case cmps1: {
				REDECLARE_REF(i, i.cmps1);
				switch (i.c) {
					case Comparison::e:	 R(i.d) = (s8)R(i.a) == (s8)R(i.b); break;
					case Comparison::ne: R(i.d) = (s8)R(i.a) != (s8)R(i.b); break;
					case Comparison::l:	 R(i.d) = (s8)R(i.a) <  (s8)R(i.b); break;
					case Comparison::le: R(i.d) = (s8)R(i.a) <= (s8)R(i.b); break;
					case Comparison::g:	 R(i.d) = (s8)R(i.a) >  (s8)R(i.b); break;
					case Comparison::ge: R(i.d) = (s8)R(i.a) >= (s8)R(i.b); break;
				}
				break;
			}
			case cmps2: {
				REDECLARE_REF(i, i.cmps2);
				switch (i.c) {
					case Comparison::e:	 R(i.d) = (s16)R(i.a) == (s16)R(i.b); break;
					case Comparison::ne: R(i.d) = (s16)R(i.a) != (s16)R(i.b); break;
					case Comparison::l:	 R(i.d) = (s16)R(i.a) <  (s16)R(i.b); break;
					case Comparison::le: R(i.d) = (s16)R(i.a) <= (s16)R(i.b); break;
					case Comparison::g:	 R(i.d) = (s16)R(i.a) >  (s16)R(i.b); break;
					case Comparison::ge: R(i.d) = (s16)R(i.a) >= (s16)R(i.b); break;
				}
				break;
			}
			case cmps4: {
				REDECLARE_REF(i, i.cmps4);
				switch (i.c) {
					case Comparison::e:	 R(i.d) = (s32)R(i.a) == (s32)R(i.b); break;
					case Comparison::ne: R(i.d) = (s32)R(i.a) != (s32)R(i.b); break;
					case Comparison::l:	 R(i.d) = (s32)R(i.a) <  (s32)R(i.b); break;
					case Comparison::le: R(i.d) = (s32)R(i.a) <= (s32)R(i.b); break;
					case Comparison::g:	 R(i.d) = (s32)R(i.a) >  (s32)R(i.b); break;
					case Comparison::ge: R(i.d) = (s32)R(i.a) >= (s32)R(i.b); break;
				}
				break;
			}
			case cmps8: {
				REDECLARE_REF(i, i.cmps8);
				switch (i.c) {
					case Comparison::e:	 R(i.d) = R(i.a) == R(i.b); break;
					case Comparison::ne: R(i.d) = R(i.a) != R(i.b); break;
					case Comparison::l:	 R(i.d) = R(i.a) <  R(i.b); break;
					case Comparison::le: R(i.d) = R(i.a) <= R(i.b); break;
					case Comparison::g:	 R(i.d) = R(i.a) >  R(i.b); break;
					case Comparison::ge: R(i.d) = R(i.a) >= R(i.b); break;
				}
				break;
			}
			case jmp: {
				REDECLARE_REF(i, i.jmp);
				rip += i.offset;
				continue;
			}
			case jz_cr: {
				REDECLARE_REF(i, i.jz_cr);
				if (R(i.reg) == 0) {
					rip += i.offset;
				} else {
					rip += 1;
				}
				continue;
			}
			case jnz_cr: {
				REDECLARE_REF(i, i.jnz_cr);
				if (R(i.reg) != 0) {
					rip += i.offset;
				} else {
					rip += 1;
				}
				continue;
			}
			case cmpstr: {
				REDECLARE_REF(i, i.cmpstr);

				auto count = R(i.d);
				auto a = Span(M(i.a), count);
				auto b = Span(M(i.b), count);

				R(i.d) = a == b;

				break;
			}
			case copyf_mmc: {
				REDECLARE_REF(i, i.copyf_mmc);
				auto d = M(i.d);
				auto s = M(i.s);
				auto size = i.size;
				memcpy(d, s, size);
				break;
			}
			case copyb_mmc: {
				REDECLARE_REF(i, i.copyb_mmc);
				auto d = M(i.d);
				auto s = M(i.s);
				auto size = i.size;
				memmove(d, s, size);
				break;
			}
			case copyf_mmr: {
				REDECLARE_REF(i, i.copyf_mmr);
				auto d = M(i.d);
				auto s = M(i.s);
				auto size = R(i.size);
				memcpy(d, s, size);
				break;
			}
			case copyb_mmr: {
				REDECLARE_REF(i, i.copyb_mmr);
				auto d = M(i.d);
				auto s = M(i.s);
				auto size = R(i.size);
				memmove(d, s, size);
				break;
			}
			case set_mcc: {
				REDECLARE_REF(i, i.set_mcc);
				auto d = M(i.d);
				auto s = i.s;
				auto size = i.size;
				memset(d, s, size);
				break;
			}

			case cmpf8: {
				REDECLARE_REF(i, i.cmpf8);
				auto d = (s64)R(i.a) - (s64)R(i.b);

				if (d == 0) cmp_flag = Comparison::e;
				if (d <  0) cmp_flag = Comparison::l;
				if (d >  0) cmp_flag = Comparison::g;

				break;
			}
			case debug_break: debug_break(); break;
			case jef_c:  { REDECLARE_REF(i, i.jef_c ); rip += (cmp_flag == Comparison::e ? i.offset : 1ll); continue; }
			case jlf_c:  { REDECLARE_REF(i, i.jlf_c ); rip += (cmp_flag == Comparison::l ? i.offset : 1ll); continue; }
			case jgf_c:  { REDECLARE_REF(i, i.jgf_c ); rip += (cmp_flag == Comparison::g ? i.offset : 1ll); continue; }
			case jnef_c: { REDECLARE_REF(i, i.jnef_c); rip += (cmp_flag != Comparison::e ? i.offset : 1ll); continue; }
			case jgef_c: { REDECLARE_REF(i, i.jgef_c); rip += (cmp_flag != Comparison::l ? i.offset : 1ll); continue; }
			case jlef_c: { REDECLARE_REF(i, i.jlef_c); rip += (cmp_flag != Comparison::g ? i.offset : 1ll); continue; }

			case add_rc: { REDECLARE_REF(i, i.add_rc); (s64 &)R(i.d) += (s64)i.s; break; }
			case sub_rc: { REDECLARE_REF(i, i.sub_rc); (s64 &)R(i.d) -= (s64)i.s; break; }
			case mul_rc: { REDECLARE_REF(i, i.mul_rc); (s64 &)R(i.d) *= (s64)i.s; break; }
			case div_rc: { REDECLARE_REF(i, i.div_rc); (s64 &)R(i.d) /= (s64)i.s; break; }
			case mod_rc: { REDECLARE_REF(i, i.mod_rc); (s64 &)R(i.d) %= (s64)i.s; break; }
			case xor_rc: { REDECLARE_REF(i, i.xor_rc); (s64 &)R(i.d) ^= (s64)i.s; break; }
			case and_rc: { REDECLARE_REF(i, i.and_rc); (s64 &)R(i.d) &= (s64)i.s; break; }
			case  or_rc: { REDECLARE_REF(i, i. or_rc); (s64 &)R(i.d) |= (s64)i.s; break; }
			case shl_rc: { REDECLARE_REF(i, i.shl_rc); (s64 &)R(i.d) <<= (s64)i.s; break; }
			case shr_rc: { REDECLARE_REF(i, i.shr_rc); (s64 &)R(i.d) >>= (s64)i.s; break; }

			case add_rr: { REDECLARE_REF(i, i.add_rr); (s64 &)R(i.d) += (s64)R(i.s); break; }
			case sub_rr: { REDECLARE_REF(i, i.sub_rr); (s64 &)R(i.d) -= (s64)R(i.s); break; }
			case mul_rr: { REDECLARE_REF(i, i.mul_rr); (s64 &)R(i.d) *= (s64)R(i.s); break; }
			case div_rr: { REDECLARE_REF(i, i.div_rr); (s64 &)R(i.d) /= (s64)R(i.s); break; }
			case mod_rr: { REDECLARE_REF(i, i.mod_rr); (s64 &)R(i.d) %= (s64)R(i.s); break; }
			case xor_rr: { REDECLARE_REF(i, i.xor_rr); (s64 &)R(i.d) ^= (s64)R(i.s); break; }
			case and_rr: { REDECLARE_REF(i, i.and_rr); (s64 &)R(i.d) &= (s64)R(i.s); break; }
			case  or_rr: { REDECLARE_REF(i, i. or_rr); (s64 &)R(i.d) |= (s64)R(i.s); break; }
			case shl_rr: { REDECLARE_REF(i, i.shl_rr); (s64 &)R(i.d) <<= (s64)R(i.s); break; }
			case shr_rr: { REDECLARE_REF(i, i.shr_rr); (s64 &)R(i.d) >>= (s64)R(i.s); break; }

			case add_mr: { REDECLARE_REF(i, i.add_mr); *(s64 *)M8(i.d) += (s64)R(i.s); break; }
			case sub_mr: { REDECLARE_REF(i, i.sub_mr); *(s64 *)M8(i.d) -= (s64)R(i.s); break; }
			case mul_mr: { REDECLARE_REF(i, i.mul_mr); *(s64 *)M8(i.d) *= (s64)R(i.s); break; }
			case div_mr: { REDECLARE_REF(i, i.div_mr); *(s64 *)M8(i.d) /= (s64)R(i.s); break; }
			case mod_mr: { REDECLARE_REF(i, i.mod_mr); *(s64 *)M8(i.d) %= (s64)R(i.s); break; }
			case xor_mr: { REDECLARE_REF(i, i.xor_mr); *(s64 *)M8(i.d) ^= (s64)R(i.s); break; }
			case and_mr: { REDECLARE_REF(i, i.and_mr); *(s64 *)M8(i.d) &= (s64)R(i.s); break; }
			case  or_mr: { REDECLARE_REF(i, i. or_mr); *(s64 *)M8(i.d) |= (s64)R(i.s); break; }
			case shl_mr: { REDECLARE_REF(i, i.shl_mr); *(s64 *)M8(i.d) <<= (s64)R(i.s); break; }
			case shr_mr: { REDECLARE_REF(i, i.shr_mr); *(s64 *)M8(i.d) >>= (s64)R(i.s); break; }

			case add_mc: { REDECLARE_REF(i, i.add_mc); *(s64 *)M8(i.d) += (s64)i.s; break; }
			case sub_mc: { REDECLARE_REF(i, i.sub_mc); *(s64 *)M8(i.d) -= (s64)i.s; break; }
			case mul_mc: { REDECLARE_REF(i, i.mul_mc); *(s64 *)M8(i.d) *= (s64)i.s; break; }
			case div_mc: { REDECLARE_REF(i, i.div_mc); *(s64 *)M8(i.d) /= (s64)i.s; break; }
			case mod_mc: { REDECLARE_REF(i, i.mod_mc); *(s64 *)M8(i.d) %= (s64)i.s; break; }
			case xor_mc: { REDECLARE_REF(i, i.xor_mc); *(s64 *)M8(i.d) ^= (s64)i.s; break; }
			case and_mc: { REDECLARE_REF(i, i.and_mc); *(s64 *)M8(i.d) &= (s64)i.s; break; }
			case  or_mc: { REDECLARE_REF(i, i. or_mc); *(s64 *)M8(i.d) |= (s64)i.s; break; }
			case shl_mc: { REDECLARE_REF(i, i.shl_mc); *(s64 *)M8(i.d) <<= (s64)i.s; break; }
			case shr_mc: { REDECLARE_REF(i, i.shr_mc); *(s64 *)M8(i.d) >>= (s64)i.s; break; }

			case movzx21_rr: { REDECLARE_REF(i, i.movzx21_rr); R(i.d) = (u8 )R(i.s); break; }
			case movzx41_rr: { REDECLARE_REF(i, i.movzx41_rr); R(i.d) = (u8 )R(i.s); break; }
			case movzx81_rr: { REDECLARE_REF(i, i.movzx81_rr); R(i.d) = (u8 )R(i.s); break; }
			case movzx42_rr: { REDECLARE_REF(i, i.movzx42_rr); R(i.d) = (u16)R(i.s); break; }
			case movzx82_rr: { REDECLARE_REF(i, i.movzx82_rr); R(i.d) = (u16)R(i.s); break; }
			case movzx84_rr: { REDECLARE_REF(i, i.movzx84_rr); R(i.d) = (u32)R(i.s); break; }
			case movsx21_rr: { REDECLARE_REF(i, i.movsx21_rr); R(i.d) = (s8 )R(i.s); break; }
			case movsx41_rr: { REDECLARE_REF(i, i.movsx41_rr); R(i.d) = (s8 )R(i.s); break; }
			case movsx81_rr: { REDECLARE_REF(i, i.movsx81_rr); R(i.d) = (s8 )R(i.s); break; }
			case movsx42_rr: { REDECLARE_REF(i, i.movsx42_rr); R(i.d) = (s16)R(i.s); break; }
			case movsx82_rr: { REDECLARE_REF(i, i.movsx82_rr); R(i.d) = (s16)R(i.s); break; }
			case movsx84_rr: { REDECLARE_REF(i, i.movsx84_rr); R(i.d) = (s32)R(i.s); break; }

			case movzx21_rm: { REDECLARE_REF(i, i.movzx21_rm); R(i.d) = *(u8  *)M(i.s); break; }
			case movzx41_rm: { REDECLARE_REF(i, i.movzx41_rm); R(i.d) = *(u8  *)M(i.s); break; }
			case movzx81_rm: { REDECLARE_REF(i, i.movzx81_rm); R(i.d) = *(u8  *)M(i.s); break; }
			case movzx42_rm: { REDECLARE_REF(i, i.movzx42_rm); R(i.d) = *(u16 *)M(i.s); break; }
			case movzx82_rm: { REDECLARE_REF(i, i.movzx82_rm); R(i.d) = *(u16 *)M(i.s); break; }
			case movzx84_rm: { REDECLARE_REF(i, i.movzx84_rm); R(i.d) = *(u32 *)M(i.s); break; }
			case movsx21_rm: { REDECLARE_REF(i, i.movsx21_rm); R(i.d) = *(s8  *)M(i.s); break; }
			case movsx41_rm: { REDECLARE_REF(i, i.movsx41_rm); R(i.d) = *(s8  *)M(i.s); break; }
			case movsx81_rm: { REDECLARE_REF(i, i.movsx81_rm); R(i.d) = *(s8  *)M(i.s); break; }
			case movsx42_rm: { REDECLARE_REF(i, i.movsx42_rm); R(i.d) = *(s16 *)M(i.s); break; }
			case movsx82_rm: { REDECLARE_REF(i, i.movsx82_rm); R(i.d) = *(s16 *)M(i.s); break; }
			case movsx84_rm: { REDECLARE_REF(i, i.movsx84_rm); R(i.d) = *(s32 *)M(i.s); break; }
			default:
				invalid_code_path();
				break;
		}
		++rip;
	}
halt:
	print("{} ips\n", total_instructions_executed / get_time(timer));
}
