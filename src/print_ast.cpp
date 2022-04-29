#include "print_ast.h"

u32 tab_count = 0;

extern bool typecheck_finished;

void print_tabs() {
	for (u32 i = tab_count; i--;) {
		print("  ");
	}
}

#define print_info(...) (print_tabs(), print(__VA_ARGS__))
//#define print_info(...)

#define print_label(...) (print_tabs(), print(__VA_ARGS__))

void print_ast(AstBinaryOperator *node);
void print_ast(AstDefinition *node);
void print_ast(AstLambda *node);
void print_ast(AstIdentifier *node);
void print_ast(AstLiteral *node);
void print_ast(AstReturn *node);
void print_ast(AstCall *node);
void print_ast(AstStruct *node);
void print_ast(AstIf *node);
void print_ast(AstIfx *node);
void print_ast(AstWhile *node);
void print_ast(AstExpressionStatement *node);
void print_ast(AstUnaryOperator *node);
void print_ast(AstSubscript *node);
void print_ast(AstTuple*node);
void print_ast(AstAssert*node);
void print_ast(AstParse* parse);
void print_ast(AstNode *node) {
	print_tabs();
	print("{}\n", node->location);
	switch (node->kind) {
		case Ast_definition: return print_ast((AstDefinition *)node);
		case Ast_lambda:     return print_ast((AstLambda     *)node);
		case Ast_identifier: return print_ast((AstIdentifier *)node);
		case Ast_literal:    return print_ast((AstLiteral    *)node);
		case Ast_return:     return print_ast((AstReturn     *)node);
		case Ast_call:       return print_ast((AstCall       *)node);
		case Ast_if:         return print_ast((AstIf         *)node);
		case Ast_ifx:        return print_ast((AstIfx        *)node);
		case Ast_while:      return print_ast((AstWhile      *)node);
		case Ast_expression_statement: return print_ast((AstExpressionStatement *)node);
		case Ast_binary_operator: return print_ast((AstBinaryOperator *)node);
		case Ast_struct: return print_ast((AstStruct *)node);
		case Ast_unary_operator: return print_ast((AstUnaryOperator *)node);
		case Ast_subscript: return print_ast((AstSubscript *)node);
		case Ast_tuple: return print_ast((AstTuple*)node);
		case Ast_assert: return print_ast((AstAssert*)node);
		case Ast_parse: return print_ast((AstParse*)node);
		default:
			print_info("unknown - uid: {}\n", node->uid());
			break;
	}
}

void print_ast(AstDefinition *node) {
	if (node->built_in)
		return;

	print_info("definition - name: {}, type: {}, uid: {}\n", node->name, type_to_string(node->type, true), node->uid());
	if (node->expression) {
		tab_count += 1;
		print_ast(node->expression);
		tab_count -= 1;
	}
}
void print_ast(AstLambda *node) {
	if (node->is_poly) {
		for (auto hardened : node->hardened_polys) {
			print_ast(hardened.lambda);
		}
	} else {
		print_info("lambda - return_type: {}, uid: {}\n", node->return_parameter ? type_to_string(node->return_parameter->type, true) : "(not defined)"str, node->uid());
		tab_count += 1;
		print_info("parameters:\n");
		tab_count += 1;
		for (auto param : node->parameters) {
			print_ast(param);
		}
		tab_count -= 1;
		print_info("statements:\n");
		tab_count += 1;
		for (auto statement : node->body_scope.statements) {
			print_ast(statement);
		}
		tab_count -= 1;
		tab_count -= 1;
	}
}
void print_ast(AstIdentifier *node) {
	print_info("identifier - type: {}, uid: {}, definition.uid: {}\n", type_to_string(node->type, true), node->uid(), node->definition ? node->definition->uid() : -1);
}
void print_ast(AstCall *node) {
	print_info("call - type: {}, uid: {}\n", type_to_string(node->type, true), node->uid());
	tab_count += 1;
		print_label("callable:\n");
		tab_count += 1;
			print_ast(node->callable);
		tab_count -= 1;
		print_label("arguments:\n");
		tab_count += 1;
		for (auto argument : node->arguments) {
			print_ast(argument.expression);
		}
		tab_count -= 1;
	tab_count -= 1;
}
void print_ast(AstBinaryOperator *node) {
	print_info("binary - operation: {}, type: {}, uid: {}\n", as_string(node->operation), type_to_string(node->type, true), node->uid());
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
	print_info("unary - operation: {}, type: {}, uid: {}\n", as_string(node->operation), type_to_string(node->type, true), node->uid());
	tab_count += 1;
	print_ast(node->expression);
	tab_count -= 1;
}
void print_ast(AstLiteral *node) {
	if (typecheck_finished) {
			 if (types_match(node->type, type_u8  )) print_info("u8  literal - value: {}, uid: {}\n", (u8 )node->integer, node->uid());
		else if (types_match(node->type, type_u16 )) print_info("u16 literal - value: {}, uid: {}\n", (u16)node->integer, node->uid());
		else if (types_match(node->type, type_u32 )) print_info("u32 literal - value: {}, uid: {}\n", (u32)node->integer, node->uid());
		else if (types_match(node->type, type_u64 )) print_info("u64 literal - value: {}, uid: {}\n", (u64)node->integer, node->uid());
		else if (types_match(node->type, type_s8  )) print_info("s8  literal - value: {}, uid: {}\n", (s8 )node->integer, node->uid());
		else if (types_match(node->type, type_s16 )) print_info("s16 literal - value: {}, uid: {}\n", (s16)node->integer, node->uid());
		else if (types_match(node->type, type_s32 )) print_info("s32 literal - value: {}, uid: {}\n", (s32)node->integer, node->uid());
		else if (types_match(node->type, type_s64 )) print_info("s64 literal - value: {}, uid: {}\n", (s64)node->integer, node->uid());
		else if (types_match(node->type, type_f32 )) print_info("f32 literal - value: {}, uid: {}\n", (f32)node->Float, node->uid());
		else if (types_match(node->type, type_f64 )) print_info("f64 literal - value: {}, uid: {}\n", (f64)node->Float, node->uid());
		else if (types_match(node->type, type_bool)) print_info("bool literal - value: {}, uid: {}\n", node->Bool, node->uid());
		else if (types_match(node->type, type_string)) print_info("string literal - value: {}, uid: {}\n", node->location, node->uid());
		else if (types_match(node->type, type_unsized_integer)) print_info("unsized integer literal - value: {}, uid: {}\n", (u64)node->integer, node->uid());
		else if (types_match(node->type, type_unsized_float)) print_info("unsized float literal - value: {}, uid: {}\n", (f64)node->Float, node->uid());
		else if (node->type->kind == Ast_unary_operator) print_info("pointer literal - value: {}, uid: {}\n", (s64)node->integer, node->uid());
		else invalid_code_path();
	} else {
		switch (node->literal_kind) {
			case LiteralKind::integer:   print_info(  "integer literal - value: {}, uid: {}\n", (s64)node->integer, node->uid()); break;
			case LiteralKind::boolean:   print_info(  "boolean literal - value: {}, uid: {}\n", node->Bool, node->uid()); break;
			case LiteralKind::string:    print_info(   "string literal - value: {}, uid: {}\n", node->location, node->uid()); break;
			case LiteralKind::Float:     print_info(    "float literal - value: {}, uid: {}\n", node->Float, node->uid()); break;
			case LiteralKind::character: print_info("character literal - value: {}, uid: {}\n", node->character, node->uid()); break;
			default: invalid_code_path();
		}
	}
}
void print_ast(AstReturn *node) {
	print_info("return - uid: {}\n", node->uid());
	if (node->expression) {
		tab_count += 1;
		print_ast(node->expression);
		tab_count -= 1;
	}
}
void print_ast(AstStruct *node) {
	if (node->definition)
		print_info("struct - name: {}, uid: {}\n", node->definition->name, node->uid());
	else
		print_info("struct - unnamed, uid: {}\n", node->uid());
	tab_count += 1;
	for_each(node->scope.definitions, [&](auto, auto member) {
		print_ast(member[0]);
	});
	tab_count -= 1;
}
void print_ast(AstIf *node) {
	print_info("if - uid: {}\n", node->uid());
	tab_count += 1;
	print_label("condition:\n");
	tab_count += 1;
	print_ast(node->condition);
	tab_count -= 1;
	print_label("true statements:\n");
	tab_count += 1;
	for (auto statement : node->true_scope.statements) {
		print_ast(statement);
	}
	tab_count -= 1;
	print_label("false statements:\n");
	tab_count += 1;
	for (auto statement : node->false_scope.statements) {
		print_ast(statement);
	}
	tab_count -= 1;
	tab_count -= 1;
}
void print_ast(AstIfx *node) {
	print_info("ifx - uid: {}\n", node->uid());
	tab_count += 1;
	print_label("condition:\n");
	tab_count += 1;
	print_ast(node->condition);
	tab_count -= 1;
	print_label("true expression:\n");
	tab_count += 1;
	print_ast(node->true_expression);
	tab_count -= 1;
	print_label("false expression:\n");
	tab_count += 1;
	print_ast(node->false_expression);
	tab_count -= 1;
	tab_count -= 1;
}
void print_ast(AstExpressionStatement *node) {
	print_ast(node->expression);
}
void print_ast(AstSubscript *node) {
	print_info("subscript - type: {}, uid: {}\n", type_to_string(node->type, true), node->uid());
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
	print_info("while - uid: {}\n", node->uid());
	tab_count += 1;
	print_label("condition:\n");
	tab_count += 1;
	print_ast(node->condition);
	tab_count -= 1;
	print_label("statements:\n");
	tab_count += 1;
	for (auto statement : node->scope.statements) {
		print_ast(statement);
	}
	tab_count -= 1;
	tab_count -= 1;
}
void print_ast(AstTuple*tuple) {
	print_info("tuple - type: {}, uid: {}\n", type_to_string(tuple->type), tuple->uid());
	tab_count += 1;
	for (auto experssion : tuple->expressions) {
		print_ast(experssion);
	}
	tab_count -= 1;
}
void print_ast(AstAssert* assert) {
	print_info("assert - uid: {}\n", assert->uid());
	tab_count += 1;
	print_ast(assert->condition);
	tab_count -= 1;
}
void print_ast(AstParse* parse) {
	print_info("parse - uid: {}\n", parse->uid());
	tab_count += 1;
	print_ast(parse->expression);
	tab_count -= 1;
}

void print_ast() {
	timed_function(context.profiler);
	for (auto statement : global_scope.statements) {
		print_ast(statement);
	}
}
