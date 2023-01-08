#include "print_ast.h"

u32 tab_count = 0;

extern bool typecheck_finished;

void print_tabs() {
	for (u32 i = tab_count; i--;) {
		print("  ");
	}
}

#define print_tabbed(...) (print_tabs(), print(__VA_ARGS__))
//#define print_tabbed(...)

#define print_label(...) (print_tabs(), print(__VA_ARGS__))

void print_ast(AstNode *node) {
	with(ConsoleColor::dark_green, print_tabbed("{}\n", node->location));
	switch (node->kind) {
		case Ast_Definition: return print_ast((AstDefinition *)node);
		case Ast_Lambda:     return print_ast((AstLambda     *)node);
		case Ast_Identifier: return print_ast((AstIdentifier *)node);
		case Ast_Literal:    return print_ast((AstLiteral    *)node);
		case Ast_Return:     return print_ast((AstReturn     *)node);
		case Ast_Call:       return print_ast((AstCall       *)node);
		case Ast_If:         return print_ast((AstIf         *)node);
		case Ast_While:      return print_ast((AstWhile      *)node);
		case Ast_Match:      return print_ast((AstMatch      *)node);
		case Ast_ExpressionStatement: return print_ast((AstExpressionStatement *)node);
		case Ast_BinaryOperator: return print_ast((AstBinaryOperator *)node);
		case Ast_Struct: return print_ast((AstStruct *)node);
		case Ast_UnaryOperator: return print_ast((AstUnaryOperator *)node);
		case Ast_Subscript: return print_ast((AstSubscript *)node);
		case Ast_Tuple: return print_ast((AstTuple*)node);
		case Ast_Assert: return print_ast((AstAssert*)node);
		case Ast_Parse: return print_ast((AstParse*)node);
		default:
			print_tabbed("unknown - uid: {}\n", node->uid);
			break;
	}
}

void print_ast(Scope *scope) {
	for (auto statement : scope->statement_list) {
		print_ast(statement);
	}
}

void print_ast(AstDefinition *node) {
	print_tabbed("definition - name: {}, type: {}, uid: {}\n", node->name, type_to_string(node->type, true), node->uid);
	if (node->expression) {
		tab_count += 1;
		print_ast(node->expression);
		tab_count -= 1;
	}
}
void print_ast(AstLambda *node) {
	if (node->is_poly) {
		for (auto hardened : node->cached_instantiations) {
			print_ast(raw(hardened.lambda));
		}
	} else {
		print_tabbed("lambda - return_type: {}, uid: {}\n", node->return_parameter ? type_to_string(node->return_parameter->type, true) : "(not defined)"str, node->uid);
		tab_count += 1;
		print_tabbed("parameters:\n");
		tab_count += 1;
		for (auto param : node->parameters) {
			print_ast(param);
		}
		tab_count -= 1;
		print_tabbed("statements:\n");
		tab_count += 1;
		//print_ast(node->body_scope);
		print_ast(node->body);
		tab_count -= 1;
		tab_count -= 1;
	}
}
void print_ast(AstIdentifier *node) {
	print_tabbed("identifier - type: {}, uid: {}, definition.uid: {}\n", type_to_string(node->type, true), node->uid, node->definition() ? node->definition()->uid : -1);
}
void print_ast(AstCall *node) {
	print_tabbed("call - type: {}, uid: {}\n", type_to_string(node->type, true), node->uid);
	tab_count += 1;
		print_label("callable:\n");
		tab_count += 1;
			print_ast(node->callable);
		tab_count -= 1;
		print_label("arguments:\n");
		tab_count += 1;
		for (auto argument : node->unsorted_arguments) {
			print_ast(argument.expression);
		}
		tab_count -= 1;
	tab_count -= 1;
}
void print_ast(AstBinaryOperator *node) {
	print_tabbed("binary - operation: {}, type: {}, uid: {}\n", as_string(node->operation), type_to_string(node->type, true), node->uid);
	tab_count += 1;

	print_label("left:\n");
	tab_count += 1;
	print_ast(node->left);
	tab_count -= 1;

	print_label("right:\n");
	tab_count += 1;
	print_ast(node->right);
	tab_count -= 1;

	tab_count -= 1;
}
void print_ast(AstUnaryOperator *node) {
	print_tabbed("unary - operation: {}, type: {}, uid: {}\n", as_string(node->operation), type_to_string(node->type, true), node->uid);
	tab_count += 1;
	print_ast(node->expression);
	tab_count -= 1;
}
void print_ast(AstLiteral *node) {
	if (typecheck_finished) {
			 if (types_match(node->type, compiler->builtin_u8  )) print_tabbed("u8  literal - value: {}, uid: {}\n", (u8 )node->integer, node->uid);
		else if (types_match(node->type, compiler->builtin_u16 )) print_tabbed("u16 literal - value: {}, uid: {}\n", (u16)node->integer, node->uid);
		else if (types_match(node->type, compiler->builtin_u32 )) print_tabbed("u32 literal - value: {}, uid: {}\n", (u32)node->integer, node->uid);
		else if (types_match(node->type, compiler->builtin_u64 )) print_tabbed("u64 literal - value: {}, uid: {}\n", (u64)node->integer, node->uid);
		else if (types_match(node->type, compiler->builtin_s8  )) print_tabbed("s8  literal - value: {}, uid: {}\n", (s8 )node->integer, node->uid);
		else if (types_match(node->type, compiler->builtin_s16 )) print_tabbed("s16 literal - value: {}, uid: {}\n", (s16)node->integer, node->uid);
		else if (types_match(node->type, compiler->builtin_s32 )) print_tabbed("s32 literal - value: {}, uid: {}\n", (s32)node->integer, node->uid);
		else if (types_match(node->type, compiler->builtin_s64 )) print_tabbed("s64 literal - value: {}, uid: {}\n", (s64)node->integer, node->uid);
		else if (types_match(node->type, compiler->builtin_f32 )) print_tabbed("f32 literal - value: {}, uid: {}\n", (f32)node->Float, node->uid);
		else if (types_match(node->type, compiler->builtin_f64 )) print_tabbed("f64 literal - value: {}, uid: {}\n", (f64)node->Float, node->uid);
		else if (types_match(node->type, compiler->builtin_bool)) print_tabbed("bool literal - value: {}, uid: {}\n", node->Bool, node->uid);
		else if (types_match(node->type, compiler->builtin_string)) print_tabbed("string literal - value: {}, uid: {}\n", node->location, node->uid);
		else if (types_match(node->type, compiler->builtin_unsized_integer)) print_tabbed("unsized integer literal - value: {}, uid: {}\n", (u64)node->integer, node->uid);
		else if (types_match(node->type, compiler->builtin_unsized_float)) print_tabbed("unsized float literal - value: {}, uid: {}\n", (f64)node->Float, node->uid);
		else if (node->type->kind == Ast_UnaryOperator) print_tabbed("pointer literal - value: {}, uid: {}\n", (s64)node->integer, node->uid);
		else invalid_code_path();
	} else {
		switch (node->literal_kind) {
			case LiteralKind::integer:   print_tabbed(  "integer literal - value: {}, uid: {}\n", (s64)node->integer, node->uid); break;
			case LiteralKind::boolean:   print_tabbed(  "boolean literal - value: {}, uid: {}\n", node->Bool, node->uid); break;
			case LiteralKind::string:    print_tabbed(   "string literal - value: {}, uid: {}\n", node->location, node->uid); break;
			case LiteralKind::Float:     print_tabbed(    "float literal - value: {}, uid: {}\n", node->Float, node->uid); break;
			case LiteralKind::character: print_tabbed("character literal - value: {}, uid: {}\n", node->character, node->uid); break;
			case LiteralKind::Struct: {
				print_tabbed("struct literal - uid: {}\n", node->uid);
				++tab_count;

				auto Struct = direct_as<AstStruct>(node->type);

				for (umm i = 0; i < node->struct_values.count; ++i) {
					auto value = node->struct_values[i];
					auto member = Struct->member_scope->definition_list[i];
					if (value) {
						print_tabbed("{}:\n", member->name);
						print_ast(value);
					} else {
						print_tabbed("{}: 0\n", member->name);
					}
				}

				--tab_count;
				break;
			}
			default: print_tabbed("unknown literal - kind: {}, uid: {}\n", node->literal_kind, node->uid); break;
		}
	}
}
void print_ast(AstReturn *node) {
	print_tabbed("return - uid: {}\n", node->uid);
	if (node->expression) {
		tab_count += 1;
		print_ast(node->expression);
		tab_count -= 1;
	}
}
void print_ast(AstStruct *node) {
	assert(node->definition);
	print_tabbed("struct - name: {}, uid: {}\n", node->definition->name, node->uid);
	tab_count += 1;
	for (auto &member : node->member_scope->statement_list) {
		print_ast(member);
	}
	tab_count -= 1;
}
void print_ast(AstIf *node) {
	print_tabbed("if - uid: {}\n", node->uid);
	tab_count += 1;
	print_label("condition:\n");
	tab_count += 1;
	print_ast(node->condition);
	tab_count -= 1;
	print_label("true statements:\n");
	tab_count += 1;
	print_ast(node->true_block);
	tab_count -= 1;
	print_label("false statements:\n");
	tab_count += 1;
	print_ast(node->false_block);
	tab_count -= 1;
	tab_count -= 1;
}
void print_ast(AstExpressionStatement *node) {
	print_ast(node->expression);
}
void print_ast(AstSubscript *node) {
	print_tabbed("subscript - type: {}, uid: {}\n", type_to_string(node->type, true), node->uid);
	tab_count += 1;
	print_label("index expression:\n");
	tab_count += 1;
	print_ast(node->index_expression);
	tab_count -= 1;
	print_label("expression:\n");
	tab_count += 1;
	print_ast(node->expression);
	tab_count -= 1;
	tab_count -= 1;
}
void print_ast(AstWhile *node) {
	print_tabbed("while - uid: {}\n", node->uid);
	tab_count += 1;
	print_label("condition:\n");
	tab_count += 1;
	print_ast(node->condition);
	tab_count -= 1;
	print_label("statements:\n");
	tab_count += 1;
	print_ast(node->scope);
	tab_count -= 1;
	tab_count -= 1;
}
void print_ast(AstTuple*tuple) {
	print_tabbed("tuple - type: {}, uid: {}\n", type_to_string(tuple->type), tuple->uid);
	tab_count += 1;
	for (auto experssion : tuple->expressions) {
		print_ast(experssion);
	}
	tab_count -= 1;
}
void print_ast(AstAssert* assert) {
	print_tabbed("assert - uid: {}\n", assert->uid);
	tab_count += 1;
	print_ast(assert->condition);
	tab_count -= 1;
}
void print_ast(AstParse* parse) {
	print_tabbed("parse - uid: {}\n", parse->uid);
	tab_count += 1;
	print_ast(parse->expression);
	tab_count -= 1;
}
void print_ast(AstMatch* match) {
	print_tabbed("match - uid: {}\n", match->uid);
	tab_count += 1;

	print_tabbed("expression:\n");
	tab_count += 1;
	print_ast(match->expression);
	tab_count -= 1;

	print_tabbed("cases:\n");
	tab_count += 1;
	u32 case_index = 0;
	for (auto &Case : match->cases) {
		defer {case_index += 1;};
		print_tabbed("case {}:\n", case_index);
		tab_count += 1;
		if (Case.expression) {
			print_tabbed("expression:\n");
			tab_count += 1;
			print_ast(Case.expression);
			tab_count -= 1;
		} else {
			print_tabbed("default case:\n");
		}

		print_tabbed("scope:\n");
		print_ast(Case.block);

		tab_count -= 1;
	}
	tab_count -= 1;

	tab_count -= 1;
}

void print_ast() {
	timed_function(compiler->profiler);
	for (auto statement : compiler->global_scope.statement_list) {
		print_ast(statement);
	}
}


void print_lowered(AstExpression *expression) {
	void print_lowered(AstBinaryOperator *);
	void print_lowered(AstLambda *);
	void print_lowered(AstLambdaType *);
	void print_lowered(AstIdentifier *);
	void print_lowered(AstLiteral *);
	void print_lowered(AstCall *);
	void print_lowered(AstStruct *);
	void print_lowered(AstEnum *);
	void print_lowered(AstIf *);
	void print_lowered(AstUnaryOperator *);
	void print_lowered(AstSubscript *);
	void print_lowered(AstTuple *);
	void print_lowered(AstArrayInitializer *);
	void print_lowered(AstSpan *);
	void print_lowered(AstBlock *);
	void print_lowered(AstMatch *);

	auto print_if_needed = [&] (char c) {
		switch (expression->kind) {
			case Ast_Identifier:
			case Ast_Literal:
			case Ast_Block:
				break;
			default:
				print(c);
				break;
		}
	};

	print_if_needed('(');
	switch (expression->kind) {
		case Ast_Lambda:           print_lowered((AstLambda           *)expression); break;
		case Ast_LambdaType:       print_lowered((AstLambdaType       *)expression); break;
		case Ast_Identifier:       print_lowered((AstIdentifier       *)expression); break;
		case Ast_Literal:          print_lowered((AstLiteral          *)expression); break;
		case Ast_Call:             print_lowered((AstCall             *)expression); break;
		case Ast_If:               print_lowered((AstIf               *)expression); break;
		case Ast_BinaryOperator:   print_lowered((AstBinaryOperator   *)expression); break;
		case Ast_Struct:           print_lowered((AstStruct           *)expression); break;
		case Ast_Enum:             print_lowered((AstEnum             *)expression); break;
		case Ast_UnaryOperator:    print_lowered((AstUnaryOperator    *)expression); break;
		case Ast_Subscript:        print_lowered((AstSubscript        *)expression); break;
		case Ast_Tuple:            print_lowered((AstTuple            *)expression); break;
		case Ast_ArrayInitializer: print_lowered((AstArrayInitializer *)expression); break;
		case Ast_Span:             print_lowered((AstSpan             *)expression); break;
		case Ast_Block:            print_lowered((AstBlock            *)expression); break;
		case Ast_Match:            print_lowered((AstMatch            *)expression); break;
		default:
			print("!unknown expression!");
			break;
	}
	print_if_needed(')');
}

void print_lowered(AstStatement *node) {
	void print_lowered(AstExpression *);
	void print_lowered(AstDefinition *);
	void print_lowered(AstReturn *);
	void print_lowered(AstWhile *);
	void print_lowered(AstExpressionStatement *);
	void print_lowered(AstAssert *);
	void print_lowered(AstParse *);
	void print_lowered(AstDefer *);
	void print_lowered(AstBlock *);
	void print_lowered(AstMatch *);
	void print_lowered(AstOperatorDefinition *);
	void print_lowered(AstLoopControl *);

	print_tabbed("");
	switch (node->kind) {
		case Ast_Definition:          return (print_lowered((AstDefinition *)node), print(";\n"), void());
		case Ast_Return:              return print_lowered((AstReturn              *)node);
		case Ast_While:               return print_lowered((AstWhile               *)node);
		case Ast_ExpressionStatement: return print_lowered((AstExpressionStatement *)node);
		case Ast_Assert:              return print_lowered((AstAssert              *)node);
		case Ast_Parse:               return print_lowered((AstParse               *)node);
		case Ast_Defer:               return print_lowered((AstDefer               *)node);
		case Ast_OperatorDefinition:  return print_lowered((AstOperatorDefinition  *)node);
		case Ast_LoopControl:         return print_lowered((AstLoopControl         *)node);
			break;
		default:
			print("!unknown statement {}!", node->uid);
			break;
	}
}

void print_lowered(Scope *scope) {
	print("{\n");
	++tab_count;
	for (auto statement : scope->statement_list) {
		print_lowered(statement);
	}
	--tab_count;
	print_tabbed("}");
}
void print_lowered(AstDefinition *definition) {
	if (!definition) {
		print("!null definition!");
		return;
	}

	if (definition->has_using)
		print("using ");

	print("{}", definition->name.count ? definition->name : "<unnamed>"str);
	//print("{}{}", definition->name.count ? definition->name : "<unnamed>"str, FormatInt{.value=definition->uid, .radix=62});

	if (definition->expression) {
		print(" :");
		if (definition->is_constant) print(": ");
		else                         print("= ");

		print_lowered(definition->expression);
	} else {
		print(": ");
		print_lowered(definition->type);
	}
}
void print_lowered(AstReturn *node) {
	if (node->expression) {
		print("return ");
		print_lowered(node->expression);
		print(";\n");
	} else {
		print("return;\n");
	}
}
void print_lowered(AstExpressionStatement *node) {
	print_lowered(node->expression);
	print(";\n");
}
void print_lowered(AstWhile *While) {
	print("while ");
	print_lowered(While->condition);
	print(" {\n");
	++tab_count;
	for (auto statement : While->scope->statement_list) {
		print_lowered(statement);
	}
	--tab_count;
	print_tabbed("}\n");
}
void print_lowered(AstAssert *assert) {
	if (assert->is_constant)
		print("#");
	print("assert ");
	print_lowered(assert->condition);
	print(";\n");
}
void print_lowered(AstParse *parse) {
	print("#parse ");
	print_lowered(parse->expression);
	print(";\n");
}
void print_lowered(AstDefer *Defer) {
	print("defer ");
	print_lowered(Defer->scope);
}
void print_lowered(AstOperatorDefinition *Operator) {
	print_lowered(raw(Operator->definition));
	print("\n");
}
void print_lowered(AstLoopControl *LoopControl) {
	switch (LoopControl->control) {
		case LoopControl::Break: println("break;"); break;
		case LoopControl::Continue: println("continue;"); break;
		default: invalid_code_path();
	}
}
void print_lowered(AstLambda *node) {
	if (node->is_poly) {
		print("<poly>{\n");
		tab_count++;
		for (auto hardened : node->cached_instantiations) {
			print_tabbed("");
			print_lowered(raw(hardened.definition));
			print('\n');
		}
		tab_count--;
		print_tabbed("}");
		return;
	}

	print("(");
	for (auto &argument : node->parameters) {
		if (&argument != node->parameters.data) {
			print(", ");
		}
		print_lowered(argument);
	}
	print("): ");
	if (node->return_parameter)
		if (node->return_parameter->name.count)
			print_lowered(raw(node->return_parameter));
		else
			print_lowered(node->return_parameter->type);
	else
		print("<unknown>");

	//print_lowered(node->body_scope);
	if (node->body) {
		print(" ");
		if (node->body->kind != Ast_Block) {
			print("=> ");
		}
		print_lowered(node->body);
	}
}
void print_lowered(AstLambdaType *node) {
	print("(#type fn (");
	for (auto &argument : node->lambda->parameters) {
		if (&argument != node->lambda->parameters.data) {
			print(", ");
		}
		print_lowered(argument->type);
	}
	print("): ");
	print_lowered(node->lambda->return_parameter->type);
	print(")");
}
void print_lowered(AstIdentifier *node) {
	print(node->name);
	//print("{}{}", node->name, FormatInt{.value=node->definition()?node->definition()->uid:-1, .radix=62});
}
void print_lowered(AstCall *node) {
	print_lowered(node->callable);
	print("(");
#if 0
	for (auto &argument : node->unsorted_arguments) {
		if (&argument != node->unsorted_arguments.data) {
			print(", ");
		}
		if (argument.expression) {
			if (!argument.name.is_empty())
				print("{} = ", argument.name);
			print_lowered(argument.expression);
		}
		else
			print("<default>");
	}
#else
	for (auto &argument : node->sorted_arguments) {
		if (&argument != node->sorted_arguments.data) {
			print(", ");
		}
		if (argument)
			print_lowered(argument);
		else
			print("<default>");
	}
#endif
	print(")");
}
void print_lowered(AstBinaryOperator *node) {
	bool has_spaces = node->operation != BinaryOperation::dot;

	print("(");
	print_lowered(node->left);
	if (has_spaces) print(" ");
	print(node->operation);
	if (has_spaces) print(" ");
	print_lowered(node->right);
	print(")");
}
void print_lowered(AstUnaryOperator *node) {
	print('(');
	switch (node->operation) {
		case UnaryOperation::autocast: {
			print('(');
			print_lowered(node->expression);
			print(") as (");
			print_lowered(node->type);
			print(')');
			break;
		}
		default: {
			print(node->operation);
			print_lowered(node->expression);
			break;
		}
	}
	print(')');
}
void print_lowered(AstLiteral *literal) {
	switch (literal->literal_kind) {
		using enum LiteralKind;
		case null: {
			print("(null as ");
			print_lowered(literal->type);
			print(")");
			break;
		}
		case integer: {
			if (types_match(literal->type, compiler->builtin_unsized_integer)) {
				print(literal->integer);
			} else {
				print("(");
				print(literal->integer);
				print(" as ");
				print_lowered(literal->type);
				print(")");
			}
			break;
		}
		case Float: {
			if (types_match(literal->type, compiler->builtin_unsized_float)) {
				print(literal->Float);
			} else {
				print("(");
				print(literal->Float);
				print(" as ");
				print_lowered(literal->type);
				print(")");
			}
			break;
		}
		case string: {
			print('"');
			print(escape_string(literal->string.get()));
			print('"');
			break;
		}
		case character: {
			print("'");
			print(as_span(encode_utf8(literal->character)));
			print("'");
			break;
		}
		case array: {
			print(".[");
			if (literal->array_elements.count) print_lowered(literal->array_elements[0]);
			for (auto e : literal->array_elements.skip(1)) print(", "), print_lowered(e);
			print("]");
			break;
		}
		case boolean: {
			print(literal->Bool);
			break;
		}
		default:
			print("!literal!");
			break;
	}
}
void print_lowered(AstStruct *node) {
	if (node->member_scope->statement_list.count) {
		print("struct {\n");
		++tab_count;
		for (auto statement : node->member_scope->statement_list) {
			print_lowered(statement);
		}
		--tab_count;
		print_tabbed("}");
	} else {
		print("struct {}");
	}
}
void print_lowered(AstEnum *Enum) {
	if (Enum->scope->statement_list.count) {
		print("enum {\n");
		++tab_count;
		for (auto statement : Enum->scope->statement_list) {
			print_lowered(statement);
		}
		--tab_count;
		print_tabbed("}");
	} else {
		print("enum {}");
	}
}
void print_lowered(AstIf *If) {
	print("if ");
	print_lowered(If->condition);
	print(" then ");
	print_lowered(raw(If->true_block));
	print(" else ");
	print_lowered(raw(If->false_block));
}
void print_lowered(AstSubscript *s) {
	if (s->is_prefix) {
		print("[");
		print_lowered(s->index_expression);
		print("]");
		print_lowered(s->expression);
	} else {
		print_lowered(s->expression);
		print("[");
		print_lowered(s->index_expression);
		print("]");
	}
}
void print_lowered(AstSpan *s) {
	print("[]");
	print_lowered(s->expression);
}
void print_lowered(AstTuple *tuple) {
	print("!tuple!");
}
void print_lowered(AstArrayInitializer *pack) {
	print(".[");
	if (pack->elements.count) print_lowered(pack->elements[0]);
	for (auto e : pack->elements.skip(1)) print(", "), print_lowered(e);
	print("]");
}
void print_lowered(AstBlock *Block) {
	print_lowered(Block->scope);
}
void print_lowered(AstMatch * match) {
	print_tabbed("match ");
	print_lowered(match->expression);

	print("{\n");
	tab_count += 1;

	for (auto Case : match->cases) {
		if (Case.expression) {
			print_tabbed("");
			print_lowered(Case.expression);
		} else {
			print_tabbed("else");
		}
		print(" => ");
		print_lowered(raw(Case.block));
	}

	tab_count -= 1;
	print_tabbed("}\n");
}

void print_lowered() {
	timed_function(compiler->profiler);
	if (compiler->print_lowered_filter.is_empty()) {
		for (auto statement : compiler->global_scope.statement_list) {
			print_lowered(statement);
		}
	} else {
		for (auto definition : compiler->global_scope.definition_list) {
			if (definition->name == compiler->print_lowered_filter)
				print_lowered(definition);
		}
	}
}
