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

void print_lowered(AstExpression *expression) {

#define e(name) void print_lowered(Ast##name *);
ENUMERATE_EXPRESSION_KIND
#undef e

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
#define e(name) case Ast_##name: print_lowered((Ast##name *)expression); break;
ENUMERATE_EXPRESSION_KIND
#undef e
		default:
			print("!unknown expression!");
			break;
	}
	print_if_needed(')');
}

void print_lowered(AstStatement *node) {
#define e(name) void print_lowered(Ast##name *);
ENUMERATE_STATEMENT_KIND
#undef e

	print_tabbed("");

	defer {
		if (node->kind == Ast_Definition) {
			print(";\n");
		}
	};

	switch (node->kind) {
#define e(name) case Ast_##name: print_lowered((Ast##name *)node); break;
ENUMERATE_STATEMENT_KIND
#undef e
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
	print(" ");
	print_lowered(While->scope);
	print("\n");
}
void print_lowered(AstFor *For) {
	print("for ");
	if (For->by_pointer)
		print("* ");
	print(For->iterator_name);
	print(" in ");
	print_lowered(For->range);
	print(" ");
	print_lowered(For->scope);
	print("\n");
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
	print("\n");
}
void print_lowered(AstTest *Test) {
	print("#compiles ");
	print_lowered(Test->scope);
	print("\n");
}
void print_lowered(AstEmptyStatement *) {
	print(";\n");
}
void print_lowered(AstUsing *Using) {
	println("using ");
	print_lowered(Using->expression);
}
void print_lowered(AstYield *Yield) {
	println("yield ");
	print_lowered(Yield->expression);
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
void print_lowered(AstForMarker *ForMarker) {
	switch (ForMarker->marker) {
		case ForMarker::it:
			println("#for_it ");
			print_lowered(ForMarker->expression);
			break;
		case ForMarker::body: println("#for_body"); break;
		case ForMarker::by_pointer: println("#for_by_pointer"); break;
		default: invalid_code_path();
	}
}
void print_lowered(AstPrint *Print) {
	print("#print ");
	print_lowered(Print->expression);
	print(";\n");
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
	if (auto ident = as<AstIdentifier>(node->callable)) {
		if (ident->name == "as") {
			if (node->sorted_arguments.count == 1) {
				print_lowered(node->sorted_arguments[0]);
				print(" as ");
				print_lowered(node->type);
				return;
			}
		}
	}

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

	print_lowered(node->left);
	if (has_spaces) print(" ");
	print(node->operation);
	if (has_spaces) print(" ");
	print_lowered(node->right);
}
void print_lowered(AstUnaryOperator *node) {
	switch (node->operation) {
		case UnaryOperation::autocast: {
			print_lowered(node->expression);
			print(" as ");
			print_lowered(node->type);
			break;
		}
		default: {
			print(node->operation);
			print_lowered(node->expression);
			break;
		}
	}
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
		print("struct ");
		print_lowered(node->member_scope);
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
	print_lowered(s->expression);
	print("[");
	print_lowered(s->index_expression);
	print("]");
}
void print_lowered(AstArray *s) {
	print("[");
	print(s->count);
	print("]");
	print_lowered(s->element_type);
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
	if (Block->scope->statement_list.count == 1) {
		if (auto est = as<AstExpressionStatement>(Block->scope->statement_list[0])) {
			print_lowered(est->expression);
			return;
		}
	}

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
void print_lowered(AstDistinct * Distinct) {
	print("#distinct ");
	print_lowered(Distinct->expression);
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
