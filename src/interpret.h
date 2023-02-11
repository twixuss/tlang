#pragma once
#include <bytecode.h>
#include <compiler.h>

#pragma warning(push, 0)
#include <tl/hash_set.h>

#define NOMINMAX
#include <Windows.h>

#include <iostream>
#include <string>
#pragma warning(pop)


inline umm append(StringBuilder &builder, Register r) {
	switch (r) {
		using enum Register;
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

inline void run_bytecode(Compiler *compiler, Span<Instruction> _instructions_, AstLambda *main_lambda, ExternLibraries extern_libraries) {
	using enum Register;

	assert(main_lambda->location_in_bytecode != -1);

	timed_function(compiler->profiler);

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
			auto relocation = binary_search(section.relocations, i, [](Relocation r) { return r.offset; });
			if (relocation) {

				assert(relocation->section == compiler->kind_of(section), "Relocations to different section is not implemented");

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

	append_section(compiler->constant_section, _constants);
	append_section(compiler->data_section, _rwdata);

	struct CallFrame {
		u8 locals   [1024*1024];
		u8 temporary[1024*1024];
	};

	CallFrame *call_frames[256] = {};
	umm top_call_frame_index = -1;


#if 1
	// register
#define R(r) (registers[(u8)(r)])
#define RU1(r) (*(u8  *)&R(r))
#define RU2(r) (*(u16 *)&R(r))
#define RU4(r) (*(u32 *)&R(r))
#define RU8(r) (*(u64 *)&R(r))
#define RS1(r) (*(s8  *)&R(r))
#define RS2(r) (*(s16 *)&R(r))
#define RS4(r) (*(s32 *)&R(r))
#define RS8(r) (*(s64 *)&R(r))
#define RF4(r) (*(f32 *)&R(r))
#define RF8(r) (*(f64 *)&R(r))

	// memory
#define M(a) ((u8 *)RU8((a).base) + RU8((a).r1) * lea_scales.data[(a).r1_scale_index] + (a).c)

	// sized memory
#define M1(a) ((u8  *)M(a))
#define M2(a) ((u16 *)M(a))
#define M4(a) ((u32 *)M(a))
#define M8(a) ((u64 *)M(a))
#define MU1(a) ((u8  *)M(a))
#define MU2(a) ((u16 *)M(a))
#define MU4(a) ((u32 *)M(a))
#define MU8(a) ((u64 *)M(a))
#define MS1(a) ((s8  *)M(a))
#define MS2(a) ((s16 *)M(a))
#define MS4(a) ((s32 *)M(a))
#define MS8(a) ((s64 *)M(a))
#define MF4(a) ((f32 *)M(a))
#define MF8(a) ((f64 *)M(a))

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

	RU8(instructions) = (u64)_instructions.data;
	RU8(constants) = (u64)array_as_span(_constants).begin();
	RU8(rwdata   ) = (u64)array_as_span(_rwdata   ).begin();
	RU8(zeros    ) = (u64)array_as_span(_zeros    ).begin();
	RU8(rb       ) = (u64)array_as_span(stack    ).end();
	RU8(rs       ) = (u64)array_as_span(stack    ).end();

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

	for_each(extern_libraries, [&](auto &kv) {
		auto &[lib, funcs] = kv;
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

	InstructionKind breaker = InstructionKind::debug_break;

	bool broken = true;

	bool debugging = false;

	auto help = [] {
		print(R"(c: continue
n: next instruction
s: show instructions
b <instruction>: break on instruction
r <index> <type>: show register value
m <address> <type>: show memory value
mr <index> <type>: show memory value by register
)");
	};

	if (debugging)
		help();

	auto poke = [&](std::string const &cmd, u64 address) {
		__try {
			if (false) {}
			else if (cmd == "u8" ) { print("{}\n", *(u8  *)address); }
			else if (cmd == "u16") { print("{}\n", *(u16 *)address); }
			else if (cmd == "u32") { print("{}\n", *(u32 *)address); }
			else if (cmd == "u64") { print("{}\n", *(u64 *)address); }
			else if (cmd == "s8" ) { print("{}\n", *(s8  *)address); }
			else if (cmd == "s16") { print("{}\n", *(s16 *)address); }
			else if (cmd == "s32") { print("{}\n", *(s32 *)address); }
			else if (cmd == "s64") { print("{}\n", *(s64 *)address); }
			else if (cmd == "f32") { print("{}\n", *(f32 *)address); }
			else if (cmd == "f64") { print("{}\n", *(f64 *)address); }
		}
		__except(EXCEPTION_EXECUTE_HANDLER) {
			print("invalid access\n");
		}
	};
	auto show = [&] {
		auto x = rip-10;
		x = max(x, _instructions.data);
		with(ConsoleColor::white,
			print_bytecode({x, (umm)(rip-x)}, x-_instructions.data)
		);
		with(ConsoleColor::green,
			print_bytecode({rip, 1}, rip-_instructions.data);
		);
	};

	bool halt = false;

	auto executable_instruction_address = [&](s64 offset) -> void * {
		auto index = offset / sizeof(Instruction);
		return 0;
	};

	while (1) {
		if (debugging) {
			if (rip->kind == breaker) {

				breaker = InstructionKind::debug_break;
				broken = true;
			}
			if (broken) {
				show();
			read_command:
				std::string cmd;
				std::cin >> cmd;

				if (cmd == "n") {
				} else if (cmd == "c") {
					broken = false;
				} else if (cmd == "h") {
					help();
					goto read_command;
				} else if (cmd == "s") {
					show();
					goto read_command;
				} else if (cmd == "b") {
					std::cin >> cmd;
#define e(name, suffix, ...) if (cmd == #name "_" #suffix) { breaker = InstructionKind::name##_##suffix; goto got_breaker_name; }
#define w(name, ...)         if (cmd == #name) { breaker = InstructionKind::name; goto got_breaker_name; }
ENUMERATE_INSTRUCTIONS
#undef e
#undef w
got_breaker_name:;
					goto read_command;
				} else if (cmd == "r") {
					u32 index;
					std::cin >> index;
					std::cin >> cmd;
					for (auto &c : cmd) c = tolower(c);

					if (false) {}
					else if (cmd == "u8" ) { print("{}\n", RU1(index)); }
					else if (cmd == "u16") { print("{}\n", RU2(index)); }
					else if (cmd == "u32") { print("{}\n", RU4(index)); }
					else if (cmd == "u64") { print("{}\n", RU8(index)); }
					else if (cmd == "s8" ) { print("{}\n", RS1(index)); }
					else if (cmd == "s16") { print("{}\n", RS2(index)); }
					else if (cmd == "s32") { print("{}\n", RS4(index)); }
					else if (cmd == "s64") { print("{}\n", RS8(index)); }
					else if (cmd == "f32") { print("{}\n", RF4(index)); }
					else if (cmd == "f64") { print("{}\n", RF8(index)); }
					goto read_command;
				} else if (cmd == "m") {
					u64 address;
					std::cin >> address;
					std::cin >> cmd;
					for (auto &c : cmd) c = tolower(c);
					poke(cmd, address);
					goto read_command;
				} else if (cmd == "mr") {
					u32 index;
					std::cin >> index;
					std::cin >> cmd;
					for (auto &c : cmd) c = tolower(c);
					poke(cmd, R(index));
					goto read_command;
				} else {
					print("bad input\n");
					goto read_command;
				}
			}
		}


		defer { total_instructions_executed++; };

		auto &i = *rip;

#if BYTECODE_DEBUG
		//if (i.comment.data)
		//	with(ConsoleColor::dark_cyan, dprint("{}\n", i.comment));
		//with(ConsoleColor::gray, print_instruction(i));
		//dprint("\n");

		//if (find(i.comment, u8"xxxyyy"s))
		//	debug_break();
#endif

		static const auto exception_filter = [](UINT code, EXCEPTION_POINTERS *exception) -> int {
			Scoped s(ConsoleColor::red);
			switch (code) {
#define c(x) case x: print(#x "\n"); break;
				c(EXCEPTION_ACCESS_VIOLATION)
				c(EXCEPTION_ARRAY_BOUNDS_EXCEEDED)
				c(EXCEPTION_BREAKPOINT)
				c(EXCEPTION_DATATYPE_MISALIGNMENT)
				c(EXCEPTION_FLT_DENORMAL_OPERAND)
				c(EXCEPTION_FLT_DIVIDE_BY_ZERO)
				c(EXCEPTION_FLT_INEXACT_RESULT)
				c(EXCEPTION_FLT_INVALID_OPERATION)
				c(EXCEPTION_FLT_OVERFLOW)
				c(EXCEPTION_FLT_STACK_CHECK)
				c(EXCEPTION_FLT_UNDERFLOW)
				c(EXCEPTION_GUARD_PAGE)
				c(EXCEPTION_ILLEGAL_INSTRUCTION)
				c(EXCEPTION_IN_PAGE_ERROR)
				c(EXCEPTION_INT_DIVIDE_BY_ZERO)
				c(EXCEPTION_INT_OVERFLOW)
				c(EXCEPTION_INVALID_DISPOSITION)
				c(EXCEPTION_INVALID_HANDLE)
				c(EXCEPTION_NONCONTINUABLE_EXCEPTION)
				c(EXCEPTION_PRIV_INSTRUCTION)
				c(EXCEPTION_SINGLE_STEP)
				c(EXCEPTION_STACK_OVERFLOW)
				c(STATUS_UNWIND_CONSOLIDATE)
#undef c
				default: print("UNKNOWN EXCEPTION\n"); break;
			}
			return EXCEPTION_EXECUTE_HANDLER;
		};

		[&] {
			__try {
				auto jump = [&](Instruction *i) {
					s64 index = i - _instructions.data;
					if ((u64)index >= _instructions.count) {
						with(ConsoleColor::red, print("INSTRUCTION OT OF BOUNDS\n"));
						print("Attempt to jump to instruction {}, actual instruction count is {}.\n", index, _instructions.count);
						debugging = true;
						broken = true;
						return false;
					}
					rip = i;
					return true;
				};
				switch (i.kind) {
					using enum InstructionKind;
					case mov_rc:  R(i.mov_rc.d) = i.mov_rc.s; break;
					case mov_rr:  R(i.mov_rr.d) = R(i.mov_rr.s); break;
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
							halt = true;
							return;
						}

						// NOTE: rip will be incremented at the end of iteration
						if (!jump((Instruction *)POP()))
							return;
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
						if (lambda->convention == CallingConvention::stdcall && !lambda->extern_library.is_empty()) {
							auto fn = (u64 (__stdcall *)(u64, u64, u64, u64, u64, u64, u64, u64, u64, u64, u64, u64)) R(i.call_r.s);

							auto p0  = *M8(rs + lambda->parameters_size - 8 *  1);
							auto p1  = *M8(rs + lambda->parameters_size - 8 *  2);
							auto p2  = *M8(rs + lambda->parameters_size - 8 *  3);
							auto p3  = *M8(rs + lambda->parameters_size - 8 *  4);
							auto p4  = *M8(rs + lambda->parameters_size - 8 *  5);
							auto p5  = *M8(rs + lambda->parameters_size - 8 *  6);
							auto p6  = *M8(rs + lambda->parameters_size - 8 *  7);
							auto p7  = *M8(rs + lambda->parameters_size - 8 *  8);
							auto p8  = *M8(rs + lambda->parameters_size - 8 *  9);
							auto p9  = *M8(rs + lambda->parameters_size - 8 * 10);
							auto p10 = *M8(rs + lambda->parameters_size - 8 * 11);
							auto p11 = *M8(rs + lambda->parameters_size - 8 * 12);

							auto retval = fn(
								p0 ,
								p1 ,
								p2 ,
								p3 ,
								p4 ,
								p5 ,
								p6 ,
								p7 ,
								p8 ,
								p9 ,
								p10,
								p11
							);
							*M8(rs + lambda->parameters_size) = retval;
						} else {
							PUSH((u64)&i);
							//auto prev_rip = rip;
							jump((Instruction *)R(i.call_r.s));
							//if (!rip) {
							//	with(ConsoleColor::red, print("Execution error:\n"));
							//	print("Attempt to call null.\n");
							//	compiler->immediate_info(prev_rip->node->location, "Caused from:");
							//	compiler->immediate_info(lambda->location, "Lambda:");
							//
							//	// FIXME: probably wanna return an error.
							//	return;
							//}
							return;
						}

						break;
					}
					case lea: {
						if (i.lea.s.base == instructions) {
							R(i.lea.d) = (u64)executable_instruction_address(i.lea.s.c);
						} else {
							R(i.lea.d) = (u64)M(i.lea.s);
						}
						dprint("loaded {} {} into {}\n", R(i.lea.d), i.lea.s, i.lea.d);
						break;
					}
					case call_c: {
						PUSH((u64)&i);

						jump(_instructions.data + i.call_c.constant);

						return;
					}
					case cmpu1: {
						REDECLARE_REF(i, i.cmpu1);
						switch (i.c) {
							case Comparison::e:	 R(i.d) = RU1(i.a) == RU1(i.b); break;
							case Comparison::ne: R(i.d) = RU1(i.a) != RU1(i.b); break;
							case Comparison::l:	 R(i.d) = RU1(i.a) <  RU1(i.b); break;
							case Comparison::le: R(i.d) = RU1(i.a) <= RU1(i.b); break;
							case Comparison::g:	 R(i.d) = RU1(i.a) >  RU1(i.b); break;
							case Comparison::ge: R(i.d) = RU1(i.a) >= RU1(i.b); break;
						}
						break;
					}
					case cmpu2: {
						REDECLARE_REF(i, i.cmpu2);
						switch (i.c) {
							case Comparison::e:	 R(i.d) = RU2(i.a) == RU2(i.b); break;
							case Comparison::ne: R(i.d) = RU2(i.a) != RU2(i.b); break;
							case Comparison::l:	 R(i.d) = RU2(i.a) <  RU2(i.b); break;
							case Comparison::le: R(i.d) = RU2(i.a) <= RU2(i.b); break;
							case Comparison::g:	 R(i.d) = RU2(i.a) >  RU2(i.b); break;
							case Comparison::ge: R(i.d) = RU2(i.a) >= RU2(i.b); break;
						}
						break;
					}
					case cmpu4: {
						REDECLARE_REF(i, i.cmpu4);
						switch (i.c) {
							case Comparison::e:	 R(i.d) = RU4(i.a) == RU4(i.b); break;
							case Comparison::ne: R(i.d) = RU4(i.a) != RU4(i.b); break;
							case Comparison::l:	 R(i.d) = RU4(i.a) <  RU4(i.b); break;
							case Comparison::le: R(i.d) = RU4(i.a) <= RU4(i.b); break;
							case Comparison::g:	 R(i.d) = RU4(i.a) >  RU4(i.b); break;
							case Comparison::ge: R(i.d) = RU4(i.a) >= RU4(i.b); break;
						}
						break;
					}
					case cmpu8: {
						REDECLARE_REF(i, i.cmpu8);
						switch (i.c) {
							case Comparison::e:	 R(i.d) = RU8(i.a) == RU8(i.b); break;
							case Comparison::ne: R(i.d) = RU8(i.a) != RU8(i.b); break;
							case Comparison::l:	 R(i.d) = RU8(i.a) <  RU8(i.b); break;
							case Comparison::le: R(i.d) = RU8(i.a) <= RU8(i.b); break;
							case Comparison::g:	 R(i.d) = RU8(i.a) >  RU8(i.b); break;
							case Comparison::ge: R(i.d) = RU8(i.a) >= RU8(i.b); break;
						}
						break;
					}
					case cmps1: {
						REDECLARE_REF(i, i.cmps1);
						switch (i.c) {
							case Comparison::e:	 R(i.d) = RS1(i.a) == RS1(i.b); break;
							case Comparison::ne: R(i.d) = RS1(i.a) != RS1(i.b); break;
							case Comparison::l:	 R(i.d) = RS1(i.a) <  RS1(i.b); break;
							case Comparison::le: R(i.d) = RS1(i.a) <= RS1(i.b); break;
							case Comparison::g:	 R(i.d) = RS1(i.a) >  RS1(i.b); break;
							case Comparison::ge: R(i.d) = RS1(i.a) >= RS1(i.b); break;
						}
						break;
					}
					case cmps2: {
						REDECLARE_REF(i, i.cmps2);
						switch (i.c) {
							case Comparison::e:	 R(i.d) = RS2(i.a) == RS2(i.b); break;
							case Comparison::ne: R(i.d) = RS2(i.a) != RS2(i.b); break;
							case Comparison::l:	 R(i.d) = RS2(i.a) <  RS2(i.b); break;
							case Comparison::le: R(i.d) = RS2(i.a) <= RS2(i.b); break;
							case Comparison::g:	 R(i.d) = RS2(i.a) >  RS2(i.b); break;
							case Comparison::ge: R(i.d) = RS2(i.a) >= RS2(i.b); break;
						}
						break;
					}
					case cmps4: {
						REDECLARE_REF(i, i.cmps4);
						switch (i.c) {
							case Comparison::e:	 R(i.d) = RS4(i.a) == RS4(i.b); break;
							case Comparison::ne: R(i.d) = RS4(i.a) != RS4(i.b); break;
							case Comparison::l:	 R(i.d) = RS4(i.a) <  RS4(i.b); break;
							case Comparison::le: R(i.d) = RS4(i.a) <= RS4(i.b); break;
							case Comparison::g:	 R(i.d) = RS4(i.a) >  RS4(i.b); break;
							case Comparison::ge: R(i.d) = RS4(i.a) >= RS4(i.b); break;
						}
						break;
					}
					case cmps8: {
						REDECLARE_REF(i, i.cmps8);
						switch (i.c) {
							case Comparison::e:	 R(i.d) = RS8(i.a) == RS8(i.b); break;
							case Comparison::ne: R(i.d) = RS8(i.a) != RS8(i.b); break;
							case Comparison::l:	 R(i.d) = RS8(i.a) <  RS8(i.b); break;
							case Comparison::le: R(i.d) = RS8(i.a) <= RS8(i.b); break;
							case Comparison::g:	 R(i.d) = RS8(i.a) >  RS8(i.b); break;
							case Comparison::ge: R(i.d) = RS8(i.a) >= RS8(i.b); break;
						}
						break;
					}
					case cmpf4: {
						REDECLARE_REF(i, i.cmpf4);
						switch (i.c) {
							case Comparison::e:	 R(i.d) = RF4(i.a) == RF4(i.b); break;
							case Comparison::ne: R(i.d) = RF4(i.a) != RF4(i.b); break;
							case Comparison::l:	 R(i.d) = RF4(i.a) <  RF4(i.b); break;
							case Comparison::le: R(i.d) = RF4(i.a) <= RF4(i.b); break;
							case Comparison::g:	 R(i.d) = RF4(i.a) >  RF4(i.b); break;
							case Comparison::ge: R(i.d) = RF4(i.a) >= RF4(i.b); break;
						}
						break;
					}
					case cmpf8: {
						REDECLARE_REF(i, i.cmpf8);
						switch (i.c) {
							case Comparison::e:	 R(i.d) = RF8(i.a) == RF8(i.b); break;
							case Comparison::ne: R(i.d) = RF8(i.a) != RF8(i.b); break;
							case Comparison::l:	 R(i.d) = RF8(i.a) <  RF8(i.b); break;
							case Comparison::le: R(i.d) = RF8(i.a) <= RF8(i.b); break;
							case Comparison::g:	 R(i.d) = RF8(i.a) >  RF8(i.b); break;
							case Comparison::ge: R(i.d) = RF8(i.a) >= RF8(i.b); break;
						}
						break;
					}
					case jmp_c: {
						REDECLARE_REF(i, i.jmp_c);
						rip += i.offset;
						return;
					}
					case jmp_r: {
						REDECLARE_REF(i, i.jmp_r);
						jump(_instructions.data + RU8(i.d));
						return;
					}
					case jz_cr: {
						REDECLARE_REF(i, i.jz_cr);
						if (R(i.reg) == 0) {
							rip += i.offset;
						} else {
							rip += 1;
						}
						return;
					}
					case jnz_cr: {
						REDECLARE_REF(i, i.jnz_cr);
						if (R(i.reg) != 0) {
							rip += i.offset;
						} else {
							rip += 1;
						}
						return;
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

					case cmpflag8: {
						REDECLARE_REF(i, i.cmpflag8);
						auto d = RS8(i.a) - RS8(i.b);

						if (d == 0) cmp_flag = Comparison::e;
						if (d <  0) cmp_flag = Comparison::l;
						if (d >  0) cmp_flag = Comparison::g;

						break;
					}
					case debug_break: if (!debugging) debug_break(); break;
					case jef_c:  { REDECLARE_REF(i, i.jef_c ); rip += (cmp_flag == Comparison::e ? i.offset : 1ll); return; }
					case jlf_c:  { REDECLARE_REF(i, i.jlf_c ); rip += (cmp_flag == Comparison::l ? i.offset : 1ll); return; }
					case jgf_c:  { REDECLARE_REF(i, i.jgf_c ); rip += (cmp_flag == Comparison::g ? i.offset : 1ll); return; }
					case jnef_c: { REDECLARE_REF(i, i.jnef_c); rip += (cmp_flag != Comparison::e ? i.offset : 1ll); return; }
					case jgef_c: { REDECLARE_REF(i, i.jgef_c); rip += (cmp_flag != Comparison::l ? i.offset : 1ll); return; }
					case jlef_c: { REDECLARE_REF(i, i.jlef_c); rip += (cmp_flag != Comparison::g ? i.offset : 1ll); return; }

					case add_rc: { REDECLARE_REF(i, i.add_rc); RS8(i.d) += (s64)i.s; break; }
					case sub_rc: { REDECLARE_REF(i, i.sub_rc); RS8(i.d) -= (s64)i.s; break; }
					case mul_rc: { REDECLARE_REF(i, i.mul_rc); RS8(i.d) *= (s64)i.s; break; }
					case divs_rc: { REDECLARE_REF(i, i.divs_rc); RS8(i.d) /= (s64)i.s; break; }
					case mods_rc: { REDECLARE_REF(i, i.mods_rc); RS8(i.d) %= (s64)i.s; break; }
					case divu_rc: { REDECLARE_REF(i, i.divu_rc); RU8(i.d) /= (u64)i.s; break; }
					case modu_rc: { REDECLARE_REF(i, i.modu_rc); RU8(i.d) %= (u64)i.s; break; }
					case xor_rc: { REDECLARE_REF(i, i.xor_rc); RS8(i.d) ^= (s64)i.s; break; }
					case and_rc: { REDECLARE_REF(i, i.and_rc); RS8(i.d) &= (s64)i.s; break; }
					case  or_rc: { REDECLARE_REF(i, i. or_rc); RS8(i.d) |= (s64)i.s; break; }
					case shl_rc: { REDECLARE_REF(i, i.shl_rc); RS8(i.d) <<= (s64)i.s; break; }
					//case shr_rc: { REDECLARE_REF(i, i.shr_rc); RS8(i.d) >>= (s64)i.s; break; }

					case add_rr: { REDECLARE_REF(i, i.add_rr); RS8(i.d) += RS8(i.s); break; }
					case sub_rr: { REDECLARE_REF(i, i.sub_rr); RS8(i.d) -= RS8(i.s); break; }
					case mul_rr: { REDECLARE_REF(i, i.mul_rr); RS8(i.d) *= RS8(i.s); break; }
					case divs_rr: { REDECLARE_REF(i, i.divs_rr); RS8(i.d) /= RS8(i.s); break; }
					case mods_rr: { REDECLARE_REF(i, i.mods_rr); RS8(i.d) %= RS8(i.s); break; }
					case divu_rr: { REDECLARE_REF(i, i.divu_rr); RU8(i.d) /= RU8(i.s); break; }
					case modu_rr: { REDECLARE_REF(i, i.modu_rr); RU8(i.d) %= RU8(i.s); break; }
					case xor_rr: { REDECLARE_REF(i, i.xor_rr); RS8(i.d) ^= RS8(i.s); break; }
					case and_rr: { REDECLARE_REF(i, i.and_rr); RS8(i.d) &= RS8(i.s); break; }
					case  or_rr: { REDECLARE_REF(i, i. or_rr); RS8(i.d) |= RS8(i.s); break; }
					case shl_rr: { REDECLARE_REF(i, i.shl_rr); RS8(i.d) <<= RS8(i.s); break; }
					//case shr_rr: { REDECLARE_REF(i, i.shr_rr); RS8(i.d) >>= RS8(i.s); break; }

					case movzx21_rr: { REDECLARE_REF(i, i.movzx21_rr); RU2(i.d) = RU1(i.s); break; }
					case movzx41_rr: { REDECLARE_REF(i, i.movzx41_rr); RU4(i.d) = RU1(i.s); break; }
					case movzx81_rr: { REDECLARE_REF(i, i.movzx81_rr); RU8(i.d) = RU1(i.s); break; }
					case movzx42_rr: { REDECLARE_REF(i, i.movzx42_rr); RU4(i.d) = RU2(i.s); break; }
					case movzx82_rr: { REDECLARE_REF(i, i.movzx82_rr); RU8(i.d) = RU2(i.s); break; }
					case movzx84_rr: { REDECLARE_REF(i, i.movzx84_rr); RU8(i.d) = RU4(i.s); break; }
					case movsx21_rr: { REDECLARE_REF(i, i.movsx21_rr); RS2(i.d) = RS1(i.s); break; }
					case movsx41_rr: { REDECLARE_REF(i, i.movsx41_rr); RS4(i.d) = RS1(i.s); break; }
					case movsx81_rr: { REDECLARE_REF(i, i.movsx81_rr); RS8(i.d) = RS1(i.s); break; }
					case movsx42_rr: { REDECLARE_REF(i, i.movsx42_rr); RS4(i.d) = RS2(i.s); break; }
					case movsx82_rr: { REDECLARE_REF(i, i.movsx82_rr); RS8(i.d) = RS2(i.s); break; }
					case movsx84_rr: { REDECLARE_REF(i, i.movsx84_rr); RS8(i.d) = RS4(i.s); break; }

					case movzx21_rm: { REDECLARE_REF(i, i.movzx21_rm); RU2(i.d) = *MU1(i.s); break; }
					case movzx41_rm: { REDECLARE_REF(i, i.movzx41_rm); RU4(i.d) = *MU1(i.s); break; }
					case movzx81_rm: { REDECLARE_REF(i, i.movzx81_rm); RU8(i.d) = *MU1(i.s); break; }
					case movzx42_rm: { REDECLARE_REF(i, i.movzx42_rm); RU4(i.d) = *MU2(i.s); break; }
					case movzx82_rm: { REDECLARE_REF(i, i.movzx82_rm); RU8(i.d) = *MU2(i.s); break; }
					case movzx84_rm: { REDECLARE_REF(i, i.movzx84_rm); RU8(i.d) = *MU4(i.s); break; }
					case movsx21_rm: { REDECLARE_REF(i, i.movsx21_rm); RS2(i.d) = *MS1(i.s); break; }
					case movsx41_rm: { REDECLARE_REF(i, i.movsx41_rm); RS4(i.d) = *MS1(i.s); break; }
					case movsx81_rm: { REDECLARE_REF(i, i.movsx81_rm); RS8(i.d) = *MS1(i.s); break; }
					case movsx42_rm: { REDECLARE_REF(i, i.movsx42_rm); RS4(i.d) = *MS2(i.s); break; }
					case movsx82_rm: { REDECLARE_REF(i, i.movsx82_rm); RS8(i.d) = *MS2(i.s); break; }
					case movsx84_rm: { REDECLARE_REF(i, i.movsx84_rm); RS8(i.d) = *MS4(i.s); break; }
					case cvt_f32_s32: { REDECLARE_REF(i, i.cvt_f32_s32); RS4(i.d) = RF4(i.d); break; }
					case cvt_f64_s64: { REDECLARE_REF(i, i.cvt_f64_s64); RS8(i.d) = RF8(i.d); break; }
					case cvt_s32_f32: { REDECLARE_REF(i, i.cvt_s32_f32); RF4(i.d) = RS4(i.d); break; }
					case cvt_s64_f64: { REDECLARE_REF(i, i.cvt_s64_f64); RF8(i.d) = RS8(i.d); break; }
					case cvt_f32_f64: { REDECLARE_REF(i, i.cvt_f32_f64); RF8(i.d) = RF4(i.d); break; }
					case cvt_f64_f32: { REDECLARE_REF(i, i.cvt_f64_f32); RF4(i.d) = RF8(i.d); break; }

					case debug_print_int: with(ConsoleColor::cyan, print("{}\n", RS8(i.debug_print_int.r))); break;

					case add4_ff: { REDECLARE_REF(i, i.add4_ff); RF4(i.d) += RF4(i.s); break; }
					case sub4_ff: { REDECLARE_REF(i, i.sub4_ff); RF4(i.d) -= RF4(i.s); break; }
					case mul4_ff: { REDECLARE_REF(i, i.mul4_ff); RF4(i.d) *= RF4(i.s); break; }
					case div4_ff: { REDECLARE_REF(i, i.div4_ff); RF4(i.d) /= RF4(i.s); break; }

					case add8_ff: { REDECLARE_REF(i, i.add8_ff); RF8(i.d) += RF8(i.s); break; }
					case sub8_ff: { REDECLARE_REF(i, i.sub8_ff); RF8(i.d) -= RF8(i.s); break; }
					case mul8_ff: { REDECLARE_REF(i, i.mul8_ff); RF8(i.d) *= RF8(i.s); break; }
					case div8_ff: { REDECLARE_REF(i, i.div8_ff); RF8(i.d) /= RF8(i.s); break; }

					//case round4_f: {
					//	REDECLARE_REF(i, i.round4_f);
					//	switch (i.mode) {
					//		using enum RoundingMode;
					//		case to_negative_infinity: RF4(i.d) = ::floorf(RF4(i.d)); break;
					//		case to_positive_infinity: RF4(i.d) = ::ceilf (RF4(i.d)); break;
					//		case to_closest_integer:   RF4(i.d) = ::roundf(RF4(i.d)); break;
					//		case to_zero:              RF4(i.d) = ::truncf(RF4(i.d)); break;
					//		default: invalid_code_path();
					//	}
					//	break;
					//}
					//case round8_f: {
					//	REDECLARE_REF(i, i.round8_f);
					//	switch (i.mode) {
					//		using enum RoundingMode;
					//		case to_negative_infinity: RF8(i.d) = ::floor(RF8(i.d)); break;
					//		case to_positive_infinity: RF8(i.d) = ::ceil (RF8(i.d)); break;
					//		case to_closest_integer:   RF8(i.d) = ::round(RF8(i.d)); break;
					//		case to_zero:              RF8(i.d) = ::trunc(RF8(i.d)); break;
					//		default: invalid_code_path();
					//	}
					//	break;
					//}

					default:
						invalid_code_path();
						break;
				}
				++rip;
			} __except(exception_filter(GetExceptionCode(), GetExceptionInformation())) {
				debugging = true;
				broken = true;
			}
		}();
		if (halt)
			break;
	}
	if (compiler->do_profile) {
		auto time = get_time(timer);
		print("Interpreted {} instruction in {} ms. ({} ips)\n", total_instructions_executed, time, total_instructions_executed / time);
	}
}
