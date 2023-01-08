#define TL_IMPL
#pragma warning(disable: 4702) // unreachable
#include <bytecode.h>
#include <compiler.h>
#include <visitor.h>
#include "msvc.h"
#include <tl/hash_set.h>

DECLARE_TARGET_INFORMATION_GETTER {
	::compiler = compiler;
	compiler->build_from = BuildFrom::ast;
	compiler->register_size = 8;
	compiler->stack_word_size = 8;
}

namespace cpp {

HashMap<AstStruct *, String> primitive_names;

HashSet<String> keywords;

void append_name(StringBuilder &builder, String name) {
	if (name.is_empty()) {
		append(builder, "_unnamed");
		return;
	}

	if (find(keywords, name)) {
		append_format(builder, "_{}", name);
		return;
	}

	for (auto c : name) {
		if (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9') || c == '_') {
			append(builder, c);
		} else {
			append(builder, '_');
		}
	}
}

void append_name(StringBuilder &builder, AstStruct *Struct) {
	assert(Struct->definition);
	append_name(builder, Struct->definition->name);
}

void append_name(StringBuilder &builder, AstLambda *Lambda) {
	if (Lambda->extern_library.count) {
		append(builder, Lambda->definition->name);
		return;
	}

	append(builder, "_");
	if (Lambda->definition) {
		append_name(builder, Lambda->definition->name);
	}
	append(builder, "_");
	append(builder, Lambda->uid);
}

void append_name(StringBuilder &builder, AstIdentifier *Identifier) {
	auto expr = Identifier->definition()->expression;
	if (expr) {
		switch (expr->kind) {
			case Ast_Lambda: return append_name(builder, (AstLambda *)expr);
			case Ast_Struct: return append_name(builder, (AstStruct *)expr);
		}
	}
	return append_name(builder, Identifier->name);
}


void append_type(StringBuilder &builder, AstExpression *type, Optional<String> name = {}, bool is_const = false) {
	auto append_const = [&] {
		if (is_const) {
			append(builder, " const ");
		}
	};
	auto append_var_name = [&] {
		append_const();
		if (name.has_value()) {
			append(builder, ' ');
			append_name(builder, name.value());
		}
	};
	switch (type->kind) {
		case Ast_Identifier: {
			append_type(builder, ((AstIdentifier *)type)->definition()->expression, name, is_const);
			break;
		}
		case Ast_Struct: {
			auto Struct = (AstStruct *)type;

			if (Struct->is_span) {
				append(builder, "_Span<");
				cpp::append_type(builder, get_span_subtype(Struct));
				append(builder, ">");
			} else {
				if (auto found = primitive_names.find(Struct)) {
					append(builder, found->value);
				} else {
					append_name(builder, Struct);
				}
			}
			append_var_name();
			break;
		}
		case Ast_Enum: {
			append(builder, "__int64");
			append_var_name();
			break;
		}
		case Ast_LambdaType: {
			// void (*)(int, float)

			auto LambdaType = (AstLambdaType *)type;
			auto lambda = LambdaType->lambda;

			switch (lambda->convention) {
				case CallingConvention::stdcall: {
					append_type(builder, lambda->return_parameter->type);
					append(builder, " (*");
					append_var_name();
					append(builder, ")(");

					for (auto &param : lambda->parameters) {
						if (&param != lambda->parameters.begin())
							append(builder, ", ");

						append_type(builder, param->type);
					}

					append(builder, ")");
					break;
				}
				case CallingConvention::tlang: {
					append(builder, "void (*");
					append_var_name();
					append(builder, ")(");

					append_type(builder, make_pointer_type(lambda->return_parameter->type));
					for (auto &param : lambda->parameters) {
						append(builder, ", ");
						append_type(builder, param->type);
					}

					append(builder, ")");
					break;
				}
				default: invalid_code_path();
			}
			break;
		}
		case Ast_UnaryOperator: {
			auto UnaryOperator = (AstUnaryOperator *)type;
			switch (UnaryOperator->operation) {
				case UnaryOperation::pointer: {
					append_type(builder, UnaryOperator->expression);
					append(builder, "*");
					append_var_name();
					break;
				}
				case UnaryOperation::option: {
					append(builder, "_Option<");
					append_type(builder, UnaryOperator->expression);
					append(builder, ">");
					append_var_name();
					break;
				}
				default: invalid_code_path();
			}
			break;
		}
		case Ast_Subscript: {
			auto Subscript = (AstSubscript *)type;
			append(builder, "_Array<");
			append_type(builder, Subscript->expression);
			append(builder, ", ");
			append(builder, (u64)get_constant_integer(Subscript->index_expression).value());
			append(builder, ">");
			append_var_name();
			break;
		}
		default: invalid_code_path();
	}
}

}

AstStruct *added;

void order_by_dependency(LinearSet<AstStruct *> &set, AstStruct *Struct) {

	for (auto &member : Struct->data_members) {
		if (auto member_struct = direct_as<AstStruct>(member->type))
			order_by_dependency(set, member_struct);
	}

	if (!Struct->is_span)
		set.add(Struct);
}

void order_by_dependency(LinearSet<AstStruct *> &structs) {
	scoped(temporary_allocator);
	LinearSet<AstStruct *> set;

	for (auto &Struct : structs) {
		order_by_dependency(set, Struct);
	}

	structs.set(set);
}

umm current_temporary_offset;

int tab_count;

void tabs(StringBuilder &builder) {
	for (int i = 0; i < tab_count; ++i) append(builder, '\t');
}

void write(StringBuilder &builder, AstReturn *Return);
void write(StringBuilder &builder, AstDefinition *Definition);
void write(StringBuilder &builder, AstLambda *Lambda);
void write(StringBuilder &builder, AstLambdaType *LambdaType);
void write(StringBuilder &builder, AstIdentifier *Identifier);
void write(StringBuilder &builder, AstBinaryOperator *BinaryOperator);
void write(StringBuilder &builder, AstUnaryOperator *UnaryOperator);
void write(StringBuilder &builder, AstPrint *Print);
void write(StringBuilder &builder, AstAssert *Assert);
void write(StringBuilder &builder, AstTest *Test);
void write(StringBuilder &builder, AstLiteral *Literal);
void write(StringBuilder &builder, AstCall *Call);
void write(StringBuilder &builder, AstStruct *Struct);
void write(StringBuilder &builder, AstEnum *Enum);
void write(StringBuilder &builder, AstIf *If);
void write(StringBuilder &builder, AstWhile *While);
void write(StringBuilder &builder, AstFor *For);
void write(StringBuilder &builder, AstExpressionStatement *ExpressionStatement);
void write(StringBuilder &builder, AstEmptyStatement *EmptyStatement);
void write(StringBuilder &builder, AstSubscript *Subscript);
void write(StringBuilder &builder, AstSpan *Span);
void write(StringBuilder &builder, AstBlock *Block);
void write(StringBuilder &builder, AstTuple *Tuple);
void write(StringBuilder &builder, AstDefer *Defer);
void write(StringBuilder &builder, AstOperatorDefinition *OperatorDefinition);
void write(StringBuilder &builder, AstParse *Parse);
void write(StringBuilder &builder, AstLoopControl *LoopControl);
void write(StringBuilder &builder, AstMatch *Match);
void write(StringBuilder &builder, AstUsing *Using);
void write(StringBuilder &builder, AstArrayInitializer *ArrayInitializer);
void write(StringBuilder &builder, AstNode *Node);
void write(StringBuilder &builder, Scope *Scope);

void write(StringBuilder &builder, AstReturn *Return) {
	if (Return->expression) {
		tabs(builder);
		append(builder, "*");
		cpp::append_name(builder, Return->lambda->return_parameter->name);
		append(builder, " = ");
		write(builder, Return->expression);
		append(builder, ";\n");
	}

	tabs(builder);
	append(builder, "return;\n");
}

void write(StringBuilder &builder, AstDefinition *Definition) {
	if (Definition->is_constant) {
		if (is_type(Definition->expression)) {
			if (Definition->expression->kind != Ast_Struct) {
				tabs(builder);
				append_format(builder, "using {} = ", Definition->name);
				cpp::append_type(builder, Definition->expression);
				append(builder, ";\n");
			}
		} else {
			tabs(builder);
			// NOTE: no const because it's pain
			if (types_match(Definition->type, compiler->builtin_unsized_integer)) {
				append_format(builder, "const S64 {}", Definition->name);
			} else {
				cpp::append_type(builder, Definition->type, Definition->name, true);
			}
			append(builder, " = ");
			write(builder, Definition->expression);
			append(builder, ";\n");
		}
	} else {
		tabs(builder);
		cpp::append_type(builder, Definition->type, Definition->name);
		append(builder, " = ");
		if (Definition->expression) {
			write(builder, Definition->expression);
		} else {
			append(builder, "{}");
		}
		append(builder, ";\n");
	}
}
void write(StringBuilder &builder, AstLambda *Lambda) {
	cpp::append_name(builder, Lambda);
}
void write(StringBuilder &builder, AstLambdaType *LambdaType) {}
void write(StringBuilder &builder, AstIdentifier *Identifier) {
	if (Identifier->definition()->definition_location == LambdaDefinitionLocation::return_parameter) {
		append(builder, "(*");
		cpp::append_name(builder, Identifier);
		append(builder, ")");
	} else {
		bool cast_const_away = Identifier->definition()->is_constant && !is_lambda(Identifier);

		if (cast_const_away) {
			append(builder, "((");
			cpp::append_type(builder, Identifier->type);
			append(builder, "&)(");
		}

		if (auto lambda = direct_as<AstLambda>(Identifier)) {
			if (lambda->has_body) {
				cpp::append_name(builder, lambda);
			} else {
				append(builder, lambda->definition->name);
			}
		} else {
			cpp::append_name(builder, Identifier);
		}

		if (cast_const_away) {
			append(builder, "))");
		}
	}
}
void write(StringBuilder &builder, AstBinaryOperator *BinaryOperator) {
	using enum BinaryOperation;

	switch (BinaryOperator->operation) {
		case dot: {
			append(builder, '(');
			write(builder, BinaryOperator->left);

			if (is_pointer(BinaryOperator->left->type))
				append(builder, "->");
			else
				append(builder, '.');
			write(builder, BinaryOperator->right);
			append(builder, ')');
			return;
		}
		case ass: {
			write(builder, BinaryOperator->left);
			append(builder, " = ");
			write(builder, BinaryOperator->right);
			return;
		}
		case as: {
			append(builder, "((");
			cpp::append_type(builder, BinaryOperator->right);
			append(builder, ")(");
			write(builder, BinaryOperator->left);
			append(builder, "))");
			return;
		}
	}

	auto name = [&] {
		switch (BinaryOperator->operation) {
#define e(x) case x: return "_" #x;
ENUMERATE_BINARY_OPERATIONS
#undef e
			default:
				invalid_code_path();
				break;
		}
	}();

	append(builder, name);
	append(builder, '(');
	write(builder, BinaryOperator->left);
	append(builder, ',');
	write(builder, BinaryOperator->right);
	append(builder, ')');
}
void write(StringBuilder &builder, AstUnaryOperator *UnaryOperator) {
	using enum UnaryOperation;
	switch (UnaryOperator->operation) {
		case internal_move_to_temporary: {
			append(builder, "_move(");
			write(builder, UnaryOperator->expression);
			append(builder, ", _tmp+");
			append(builder, current_temporary_offset);
			append(builder, ")");

			current_temporary_offset += get_size(UnaryOperator->expression->type);
			return;
		}
	}

	auto name = [&] {
		using enum UnaryOperation;
		switch (UnaryOperator->operation) {
#define e(x) case x: return "_" #x;
ENUMERATE_UNARY_OPERATIONS
#undef e
			default:
				invalid_code_path();
				break;
		}
	}();

	append(builder, name);
	append(builder, '(');
	write(builder, UnaryOperator->expression);
	append(builder, ')');
}
void write(StringBuilder &builder, AstPrint *Print) { not_implemented(); }
void write(StringBuilder &builder, AstAssert *Assert) {
	tabs(builder);
	append(builder, "_assert(");
	write(builder, Assert->condition);
	append_format(builder, "); // Assertion failed: {}\n", Assert->message);
}
void write(StringBuilder &builder, AstTest *Test) { not_implemented(); }
void write(StringBuilder &builder, AstLiteral *Literal) {
	switch (Literal->literal_kind) {
		case LiteralKind::null:
			if (is_pointer(Literal->type))
				append(builder, "0");
			else
				append(builder, "{}");
			break;
		case LiteralKind::integer:
			append(builder, (u64)Literal->integer);
			break;
		case LiteralKind::boolean:
			append(builder, Literal->Bool ? "true" : "false");
			break;
		case LiteralKind::string:
			//append_format(builder, "String{{.data=((U8*)_constants)+{},.count={}}}", Literal->string.offset, Literal->string.count);
			append_format(builder, "_make_String(((U8*)_constants)+{},{})", Literal->string.offset, Literal->string.count);
			break;
		case LiteralKind::character:
			append(builder, "'\\x");
			append(builder, FormatInt{.value = Literal->character, .radix = 16, .leading_zero_count = 2});
			append(builder, '\'');
			break;
		case LiteralKind::Float:
			append(builder, Literal->Float);
			break;
		case LiteralKind::Struct:
			append(builder, "*(");
			cpp::append_type(builder, Literal->type);
			append_format(builder, "*)(((U8*)_constants)+{})", Literal->struct_offset);
			break;
		default:
			invalid_code_path();
			break;
	}
}
void write(StringBuilder &builder, AstCall *Call) {
	if (Call->lambda_type) {
		// Lambda
		switch (Call->lambda_type->lambda->convention) {
			case CallingConvention::stdcall: {
				write(builder, Call->callable);
				append(builder, '(');
				for (auto &arg : Call->sorted_arguments) {
					if (&arg != Call->sorted_arguments.begin())
						append(builder, ',');

					write(builder, arg);
				}
				append(builder, ')');
				break;
			}
			case CallingConvention::tlang: {
				append(builder, "_call(");
				write(builder, Call->callable);
				for (auto &arg : Call->sorted_arguments) {
					append(builder, ',');
					write(builder, arg);
				}
				append(builder, ')');
				break;
			}
			default: {
				invalid_code_path();
			}
		}
	} else {
		// Struct
		append(builder, "_make_");
		cpp::append_name(builder, direct_as<AstStruct>(Call->callable));
		append(builder, '(');
		for (auto &arg : Call->sorted_arguments) {
			if (&arg != Call->sorted_arguments.begin())
				append(builder, ',');

			write(builder, arg);
		}
		append(builder, ')');
	}
}
void write(StringBuilder &builder, AstStruct *Struct) { not_implemented(); }
void write(StringBuilder &builder, AstEnum *Enum) { not_implemented(); }
void write(StringBuilder &builder, AstIf *If) {
	tabs(builder);
	append(builder, "if (");
	write(builder, If->condition);
	append(builder, ") {\n");

	{
		++tab_count;
		defer { --tab_count; };

		write(builder, If->true_block);
	}

	if (If->false_block->scope->statement_list.count == 0) {
		tabs(builder);
		append(builder, "}\n");
	} else {
		tabs(builder);
		append(builder, "} else {\n");

		{
			++tab_count;
			defer { --tab_count; };

			write(builder, If->false_block);
		}

		tabs(builder);
		append(builder, "}\n");
	}
}
void write(StringBuilder &builder, AstWhile *While) {
	tabs(builder);
	append(builder, "while (");
	write(builder, While->condition);
	append(builder, ") {\n");

	{
		++tab_count;
		defer { --tab_count; };

		write(builder, While->scope);
	}

	tabs(builder);
	append(builder, "}\n");
}
void write(StringBuilder &builder, AstFor *For) { not_implemented(); }
void write(StringBuilder &builder, AstExpressionStatement *ExpressionStatement) {
	tabs(builder);
	write(builder, ExpressionStatement->expression);
	append(builder, ";\n");
}
void write(StringBuilder &builder, AstEmptyStatement *EmptyStatement) { not_implemented(); }
void write(StringBuilder &builder, AstSubscript *Subscript) {
	append(builder, "_subscript(");
	write(builder, Subscript->expression);
	append(builder, ',');
	write(builder, Subscript->index_expression);
	append(builder, ')');
}
void write(StringBuilder &builder, AstSpan *Span) { not_implemented(); }
void write(StringBuilder &builder, AstBlock *Block) {
	tabs(builder);
	append(builder, "{\n");

	{
		++tab_count;
		defer { --tab_count; };

		write(builder, Block->scope);
	}

	tabs(builder);
	append(builder, "}\n");
}
void write(StringBuilder &builder, AstTuple *Tuple) { not_implemented(); }
void write(StringBuilder &builder, AstDefer *Defer) { not_implemented(); }
void write(StringBuilder &builder, AstOperatorDefinition *OperatorDefinition) { not_implemented(); }
void write(StringBuilder &builder, AstParse *Parse) { not_implemented(); }
void write(StringBuilder &builder, AstLoopControl *LoopControl) {
	tabs(builder);
	switch (LoopControl->control) {
		case LoopControl::Break: append(builder, "break"); break;
		case LoopControl::Continue: append(builder, "continue"); break;
		default: invalid_code_path(); break;
	}
	append(builder, ";\n");
}
void write(StringBuilder &builder, AstMatch *Match) {
	tabs(builder);
	append(builder, "switch (");
	write(builder, Match->expression);
	append(builder, ") {\n");

	{
		++tab_count;
		defer { --tab_count; };

		for (auto &Case : Match->cases) {
			tabs(builder);
			append(builder, "case ");
			write(builder, Case.expression);
			append(builder, ":{\n");

			{
				++tab_count;
				defer { --tab_count; };

				write(builder, Case.scope);
			}
			tabs(builder);
			append(builder, "break;}\n");
		}
	}

	tabs(builder);
	append(builder, "}\n");
}
void write(StringBuilder &builder, AstUsing *Using) { not_implemented(); }
void write(StringBuilder &builder, AstArrayInitializer *ArrayInitializer) { not_implemented(); }
void write(StringBuilder &builder, Scope *Scope) {
	for (auto statement : Scope->statement_list) {
		write(builder, statement);
	}
}

void write(StringBuilder &builder, AstNode *node) {
	switch (node->kind) {
#define e(x) case Ast_##x: return write(builder, (Ast##x *)node);
		ENUMERATE_AST_KIND
#undef e
		default:
			invalid_code_path();
			break;
	}
}

bool is_fundamental(AstExpression *type) {
	return
		types_match(type, compiler->builtin_void) ||
		types_match(type, compiler->builtin_bool) ||
		types_match(type, compiler->builtin_u8) ||
		types_match(type, compiler->builtin_u16) ||
		types_match(type, compiler->builtin_u32) ||
		types_match(type, compiler->builtin_u64) ||
		types_match(type, compiler->builtin_s8) ||
		types_match(type, compiler->builtin_s16) ||
		types_match(type, compiler->builtin_s32) ||
		types_match(type, compiler->builtin_s64) ||
		types_match(type, compiler->builtin_f32) ||
		types_match(type, compiler->builtin_f64);
}

DECLARE_OUTPUT_BUILDER {
	init_allocator();
	init_printer();

	timed_function(compiler->profiler);

	construct(cpp::keywords);
	cpp::keywords.insert("this"str);
	cpp::keywords.insert("struct"str);
	cpp::keywords.insert("class"str);
	cpp::keywords.insert("unsigned"str);
	cpp::keywords.insert("signed"str);
	cpp::keywords.insert("void"str);
	cpp::keywords.insert("bool"str);
	cpp::keywords.insert("char"str);
	cpp::keywords.insert("short"str);
	cpp::keywords.insert("int"str);
	cpp::keywords.insert("long"str);
	cpp::keywords.insert("float"str);
	cpp::keywords.insert("double"str);

	construct(cpp::primitive_names);
	cpp::primitive_names.get_or_insert(compiler->builtin_void.Struct) = "void"str;
	cpp::primitive_names.get_or_insert(compiler->builtin_bool.Struct) = "bool"str;
	cpp::primitive_names.get_or_insert(compiler->builtin_u8.Struct)   = "U8"str;
	cpp::primitive_names.get_or_insert(compiler->builtin_u16.Struct)  = "U16"str;
	cpp::primitive_names.get_or_insert(compiler->builtin_u32.Struct)  = "U32"str;
	cpp::primitive_names.get_or_insert(compiler->builtin_u64.Struct)  = "U64"str;
	cpp::primitive_names.get_or_insert(compiler->builtin_s8.Struct)   = "S8"str;
	cpp::primitive_names.get_or_insert(compiler->builtin_s16.Struct)  = "S16"str;
	cpp::primitive_names.get_or_insert(compiler->builtin_s32.Struct)  = "S32"str;
	cpp::primitive_names.get_or_insert(compiler->builtin_s64.Struct)  = "S64"str;


	StringBuilder cpp_builder;

	LinearSet<AstStruct *> all_structs;
	LinearSet<AstLambda *> all_lambdas_with_body;
	LinearSet<AstLambda *> all_extern_lambdas;
	LinearSet<AstDefinition *> readonly_definitions;
	LinearSet<AstDefinition *> readwrite_definitions;

	for (auto statement : global_statements) {
		visit(statement, Combine {
			[&] (AstNode *Node) {},
			[&] (AstStruct *Struct) {
				if (Struct->is_template) {
					for (auto instantiation : Struct->instantiations) {
						all_structs.add(instantiation.Struct);
					}
				} else {
					all_structs.add(Struct);
				}
			},
			[&] (AstLambda *Lambda) {
				if (Lambda->has_body) {
					if (Lambda->is_poly) {
						for (auto &instantiation : Lambda->cached_instantiations)
							all_lambdas_with_body.add(instantiation.lambda);
					} else {
						all_lambdas_with_body.add(Lambda);
					}
				} else {
					all_extern_lambdas.add(Lambda);
				}
			},
			[&] (AstEnum *Enum) {}
		});

		if (statement->kind == Ast_Definition) {
			auto definition = (AstDefinition *)statement;
			if (definition->is_constant) {
				readonly_definitions.add(definition);
			} else {
				readwrite_definitions.add(definition);
			}
		}
	}

	order_by_dependency(all_structs);

	for (auto &Struct : all_structs) {
		if (is_fundamental(Struct))
			continue;

		append(cpp_builder, Struct->layout == StructLayout::c && !Struct->is_union ? "struct" : "union");
		append(cpp_builder, ' ');
		cpp::append_name(cpp_builder, Struct);
		append(cpp_builder, ";\n");
	}

	for (auto &Struct : all_structs) {
		if (is_fundamental(Struct))
			continue;

		if (Struct->layout == StructLayout::c) {
			append(cpp_builder, Struct->is_union ? "union " : "struct ");
			cpp::append_name(cpp_builder, Struct);
			append(cpp_builder, " {\n");

			for (auto &member : Struct->data_members) {
				append(cpp_builder, "\t");
				cpp::append_type(cpp_builder, member->type, member->name);
				append(cpp_builder, ";\n");
			}
			append(cpp_builder, "};\n");
		} else if (Struct->layout == StructLayout::tlang) {
			append(cpp_builder, "union ");
			cpp::append_name(cpp_builder, Struct);
			append(cpp_builder, " {\n");

			int pad_index = 0;
			for (auto &member : Struct->data_members) {
				if (member->offset == 0) {
					append(cpp_builder, "\t");
					cpp::append_type(cpp_builder, member->type, member->name);
					append(cpp_builder, ";\n");
				} else {
					append_format(cpp_builder, "\tstruct{{ char pad{}[{}]; ", pad_index++, member->offset);
					cpp::append_type(cpp_builder, member->type, member->name);
					append(cpp_builder, ";};\n");
				}
			}
			append(cpp_builder, "};\n");
		} else {
			invalid_code_path();
		}
	}


	for (auto &Struct : all_structs) {
		if (is_fundamental(Struct))
			continue;

		if (Struct->layout == StructLayout::tlang) {
			cpp::append_type(cpp_builder, Struct);
			append(cpp_builder, " _make_");
			cpp::append_name(cpp_builder, Struct);
			append(cpp_builder, "(");

			int pad_index = 0;
			for (auto &member : Struct->data_members) {
				if (&member != Struct->data_members.begin()) {
					append(cpp_builder, ", ");
				}

				append(cpp_builder, "_Option<");
				cpp::append_type(cpp_builder, member->type);
				append(cpp_builder, "> ");
				append(cpp_builder, member->name);
			}
			append(cpp_builder, "){\n\t");
			cpp::append_type(cpp_builder, Struct, "result"str);
			append(cpp_builder, ";\n");

			for (auto &member : Struct->data_members) {
				append_format(cpp_builder, "\tif({}.has_value)result.{} = {}.value;\n", member->name, member->name, member->name);
			}

			append(cpp_builder, "\treturn result;\n}\n");
		}
	}


	append(cpp_builder, "const U64 _constants[]{");
	{
		for (auto &r : compiler->constant_section.relocations)
			assert(r % 8 == 0, "Relocation was not aligned to 8 bytes.");

		umm i = 0;
		auto *r = compiler->constant_section.relocations.begin();
		while (i < compiler->constant_section.buffer.count) {
			auto data = *(u64 *)&compiler->constant_section.buffer.data[i];
			if (r < compiler->constant_section.relocations.end() && i == *r) {
				append_format(cpp_builder, "(U64)(_constants+0x{})", FormatInt{.value=data,.radix=16});
				++r;
			} else {
				append_format(cpp_builder, "0x{}", FormatInt{.value=data,.radix=16});
			}
			i += 8;
			append(cpp_builder, ',');
		}
	}
	append(cpp_builder, "};\n");


	for (auto &definition : readonly_definitions) {
		if (is_type(definition->expression)) {
			if (definition->expression->kind != Ast_Struct) {
				append_format(cpp_builder, "using {} = ", definition->name);
				cpp::append_type(cpp_builder, definition->expression);
				append(cpp_builder, ";\n");
			}
		} else {
			if (definition->evaluated) {
				auto type = definition->type;
				if (types_match(type, compiler->builtin_unsized_integer))
					type = compiler->builtin_default_integer->ident;

				// NOTE: no const because it's pain
				cpp::append_type(cpp_builder, type, definition->name, true);
				append(cpp_builder, " = (");
				cpp::append_type(cpp_builder, type);
				append(cpp_builder, ")(");
				write(cpp_builder, definition->evaluated);
				append(cpp_builder, ");\n");
			}
		}
	}

	for (auto &definition : readwrite_definitions) {
		write(cpp_builder, definition);
	}

	auto declare_stdcall = [&] (AstLambda *lambda) {
		append_format(cpp_builder, "auto {}(", lambda->definition->name);


		for (auto &param : lambda->parameters) {
			if (&param != lambda->parameters.begin())
				append(cpp_builder, ", ");
			cpp::append_type(cpp_builder, param->type, param->name);
		}

		append(cpp_builder, ")->");
		cpp::append_type(cpp_builder, lambda->return_parameter->type);
		append(cpp_builder, ";\n");
	};

	auto declare_tlang = [&] (AstLambda *lambda) {
		append(cpp_builder, "void ");
		if (lambda->has_body)
			cpp::append_name(cpp_builder, lambda);
		else
			append(cpp_builder, lambda->definition->name);

		append(cpp_builder, "(");

		cpp::append_type(cpp_builder, make_pointer_type(lambda->return_parameter->type), lambda->return_parameter->name);

		for (auto &param : lambda->parameters) {
			append(cpp_builder, ", ");
			cpp::append_type(cpp_builder, param->type, param->name);
		}

		append(cpp_builder, ");\n");
	};

	append(cpp_builder, "extern \"C\" {\n");

	for (auto &lambda : all_extern_lambdas) {
		if (lambda->convention != CallingConvention::tlang) {
			declare_stdcall(lambda);
		}
	}

	append(cpp_builder, "}\n");

	for (auto &lambda : all_extern_lambdas) {
		if (lambda->convention == CallingConvention::tlang) {
			declare_tlang(lambda);
		}
	}

	for (auto &lambda : all_lambdas_with_body) {
		declare_tlang(lambda);
	}

	for (auto &lambda : all_lambdas_with_body) {
		current_temporary_offset = 0;
		tab_count = 1;

		//
		// Build body
		//
		StringBuilder body_builder;
		for (auto statement : lambda->body_scope->statement_list) {
			write(body_builder, statement);
		}

		//
		// Append body to global builder
		//
		append(cpp_builder, "void ");
		cpp::append_name(cpp_builder, lambda);

		append(cpp_builder, "(");

		cpp::append_type(cpp_builder, make_pointer_type(lambda->return_parameter->type), lambda->return_parameter->name);

		for (auto &param : lambda->parameters) {
			append(cpp_builder, ", ");
			cpp::append_type(cpp_builder, param->type, param->name);
		}

		append(cpp_builder, ") {\n\t_init(");

		cpp::append_name(cpp_builder, lambda->return_parameter->name);
		append(cpp_builder, ");\n");

		if (current_temporary_offset) {
			append_format(cpp_builder, "\tU8 _tmp[{}];\n", current_temporary_offset);
		}

		append(cpp_builder, body_builder);

		append(cpp_builder, "}\n");
	}

	append(cpp_builder, "int main(){\n\tU64 x;\n\t");
	cpp::append_name(cpp_builder, compiler->init_runtime_lambda);
	append(cpp_builder, "(&x);\n\treturn ");
	cpp::append_name(cpp_builder, compiler->main_lambda);
	append(cpp_builder, "(&x);\n}\n");

	auto output_path_base = format("{}\\{}", compiler->current_directory, parse_path(compiler->source_path).name);
	auto cpp_path = to_pathchars(format(u8"{}.cpp", output_path_base));

	{
		auto file = open_file(cpp_path, {.write = true});
		defer { close(file); };

		withs(temporary_allocator) {
			write(file, read_entire_file(format("{}\\targets\\cpp_x86_64_windows\\prelude.h", compiler->compiler_directory)));
			write(file, to_string(cpp_builder));
			write(file, read_entire_file(format("{}\\targets\\cpp_x86_64_windows\\footer.h", compiler->compiler_directory)));
		};
	}

	{
		scoped_phase("Assembling and linking");

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

		StringBuilder bat_builder;

		append_format(bat_builder, "@echo off\n");
		append_format(bat_builder,
			R"("{}\bin\Hostx64\x64\cl.exe" /nologo "{}.cpp" /ZI /I"{}/include" /std:c++20 /D"_MT" /link /out:"{}" /nodefaultlib /subsystem:console /DEBUG:FULL /LIBPATH:"{}\um\x64"  /LIBPATH:"{}\ucrt\x64" /LIBPATH:"{}\libs" /LIBPATH:"{}\lib\x64" kernel32.lib libcmtd.lib libvcruntimed.lib libcpmtd.lib libucrtd.lib)",
			msvc_directory,
			output_path_base,
			msvc_directory,
			compiler->output_path,
			wkits_directory,
			wkits_directory,
			compiler->compiler_directory,
			msvc_directory
		);
		for_each(compiler->extern_libraries, [&](auto library, auto) {
			append_format(bat_builder, " {}.lib", library);
		});

		auto bat_path = format(u8"{}.build.bat"s, output_path_base);
		write_entire_file(bat_path, as_bytes(to_string(bat_builder)));
#if 1
		timed_block(compiler->profiler, "msvc"s);

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
		delete_file(cpp_path);


	return true;
}
