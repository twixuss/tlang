#include <ast.h>

umm append(StringBuilder &builder, AstKind kind) {
	switch (kind) {
#define e(name) case Ast_ ## name: return append(builder, u8#name##s);
		ENUMERATE_AST_KIND(e)
#undef e
	}
	return append(builder, "(unknown AstKind)");
}

s32 ast_node_uid_counter;

AstStruct type_bool;
AstStruct type_u8;
AstStruct type_u16;
AstStruct type_u32;
AstStruct type_u64;
AstStruct type_s8;
AstStruct type_s16;
AstStruct type_s32;
AstStruct type_s64;
AstStruct type_f32;
AstStruct type_f64;
AstStruct type_type;
AstStruct type_string;
AstStruct type_noinit;
AstStruct type_unsized_integer;
AstStruct type_unsized_float;
AstStruct type_void;
AstStruct *type_default_integer;
AstStruct *type_default_float;
AstUnaryOperator type_pointer_to_void;

bool struct_is_built_in(AstStruct *type) {
	return
		type == &type_bool   ||
		type == &type_u8     ||
		type == &type_u16    ||
		type == &type_u32    ||
		type == &type_u64    ||
		type == &type_s8     ||
		type == &type_s16    ||
		type == &type_s32    ||
		type == &type_s64    ||
		type == &type_f32    ||
		type == &type_f64    ||
		type == &type_type   ||
		type == &type_string ||
		type == &type_noinit ||
		type == &type_unsized_integer ||
		type == &type_unsized_float ||
		type == &type_void;
}
bool type_is_built_in(AstExpression *type) {
	switch (type->kind) {
		case Ast_struct: return struct_is_built_in((AstStruct *)type);
		case Ast_unary_operator: return true;
		case Ast_subscript: return true;
	}
	invalid_code_path();
}

Scope global_scope;

Mutex global_scope_mutex;
void lock(Scope *scope) {
	if (scope == &global_scope) {
		lock(global_scope_mutex);
	}
}
void unlock(Scope *scope) {
	if (scope == &global_scope) {
		unlock(global_scope_mutex);
	}
}

HashMap<Span<utf8>, AstDefinition *> names_not_available_for_globals;

bool needs_semicolon(AstExpression *node) {
	// if (node->kind == Ast_unary_operator)
	// 	return needs_semicolon(((AstUnaryOperator *)node)->expression);

	if (node->kind == Ast_lambda && ((AstLambda *)node)->has_body == false)
		return true;

	return node->kind != Ast_lambda && node->kind != Ast_struct;
}

bool can_be_global(AstStatement *statement) {
	if (statement->kind == Ast_definition)
		return true;
	return false;
}

Optional<BigInt> get_constant_integer(AstExpression *expression) {
	switch (expression->kind) {
		case Ast_literal: {
			auto literal = (AstLiteral *)expression;
			if (literal->literal_kind == LiteralKind::integer) {
				return literal->integer;
			}
			break;
		}
		case Ast_identifier: {
			auto ident = (AstIdentifier *)expression;
			if (ident->definition && ident->definition->is_constant) {
				return get_constant_integer(ident->definition->expression);
			}
			break;
		}
		case Ast_unary_operator: {
			auto unop = (AstUnaryOperator *)expression;
			auto got_value = get_constant_integer(unop->expression);
			if (!got_value)
				return got_value;

			auto value = got_value.value_unchecked();

			switch (unop->operation) {
				case '-': return -value;
			}
			break;
		}
		case Ast_binary_operator: {
			auto binop = (AstBinaryOperator *)expression;
			using enum BinaryOperation;
			switch (binop->operation) {
				case add: return get_constant_integer(binop->left) + get_constant_integer(binop->right);
				case sub: return get_constant_integer(binop->left) - get_constant_integer(binop->right);
				case mul: return get_constant_integer(binop->left) * get_constant_integer(binop->right);
				case div: print("constexpr / not implemented\n"); return {};// return get_constant_integer(binop->left) / get_constant_integer(binop->right);
				case mod: print("constexpr % not implemented\n"); return {};// return get_constant_integer(binop->left) % get_constant_integer(binop->right);
				case bxor: return get_constant_integer(binop->left) ^ get_constant_integer(binop->right);
				case band: return get_constant_integer(binop->left) & get_constant_integer(binop->right);
				case bor: return get_constant_integer(binop->left) | get_constant_integer(binop->right);
				case bsl: print("constexpr << not implemented\n"); return {};// return get_constant_integer(binop->left) << get_constant_integer(binop->right);
				case bsr: print("constexpr >> not implemented\n"); return {};// return get_constant_integer(binop->left) >> get_constant_integer(binop->right);
				case eq:
				case ne:
				case dot:
					return {};
				default: invalid_code_path();
			}
			break;
		}
	}
	return {};
}

bool is_type(AstExpression *expression) {
	if (expression->type == &type_type)
		return true;

	if (expression->kind == Ast_lambda)
		return ((AstLambda *)expression)->is_type;

	return false;
}

void append_type(StringBuilder &builder, AstExpression *type, bool silent_error) {
#define ensure(x) \
	if (silent_error) { \
		if (!(x)) { \
			append(builder, u8"!error!"s); \
			return; \
		} \
	} else { \
		assert(x); \
	}

	ensure(is_type(type));
	switch (type->kind) {
		case Ast_struct: {
			auto Struct = (AstStruct *)type;
			append(builder, Struct->definition->name);
			break;
		}
		case Ast_lambda: {
			auto lambda = (AstLambda *)type;

			append(builder, "fn ");
			switch (lambda->convention) {
				case CallingConvention::stdcall: append(builder, "#stdcall "); break;
			}
			append(builder, "(");
			for (auto &parameter : lambda->parameters) {
				if (&parameter != lambda->parameters.data) {
					append(builder, ", ");
				}
				append_type(builder, parameter->type, silent_error);
			}
			append(builder, ") -> ");
			append_type(builder, lambda->return_parameter->type, silent_error);
			break;
		}
		case Ast_identifier: {
			auto identifier = (AstIdentifier *)type;
			ensure(identifier->definition);
			ensure(identifier->definition->expression->type == &type_type);
			append(builder, identifier->name);
			break;
		}
		case Ast_unary_operator: {
			auto unop = (AstUnaryOperator *)type;
			ensure(unop->operation == '*');
			append(builder, '*');
			append_type(builder, unop->expression, silent_error);
			break;
		}
		case Ast_subscript: {
			auto subscript = (AstSubscript *)type;
			append(builder, '[');
			append(builder, (s64)get_constant_integer(subscript->index_expression).value());
			append(builder, ']');
			append_type(builder, subscript->expression, silent_error);
			break;
		}
		default: {
			ensure(false);
		}
	}
#undef ensure
}

List<utf8> type_to_string(AstExpression *type, bool silent_error) {
	if (!type)
		return to_list(u8"null"s);

	StringBuilder builder;
	append_type(builder, type, silent_error);
	return (List<utf8>)to_string(builder);
}

s64 get_size(AstExpression *type) {
	assert(type);
	switch (type->kind) {
		case Ast_struct: {
			auto Struct = (AstStruct *)type;
			return Struct->size;
		}
		case Ast_identifier: {
			auto identifier = (AstIdentifier *)type;
			return get_size(identifier->definition->expression);
		}
		case Ast_unary_operator: {
			auto unop = (AstUnaryOperator *)type;
			switch (unop->operation) {
				case '*': return 8;
				default: invalid_code_path();
			}
		}
		case Ast_subscript: {
			auto subscript = (AstSubscript *)type;

			auto count = get_constant_integer(subscript->index_expression);
			assert(count.has_value());

			return get_size(subscript->expression) * (s64)count.value();
		}
		case Ast_lambda: {
			return 8;
		}
		default: {
			invalid_code_path();
			return 0;
		}
	}
}

s64 get_align(AstExpression *type) {
	assert(type);
	switch (type->kind) {
		case Ast_struct: {
			auto Struct = (AstStruct *)type;
			return Struct->alignment;
		}
		case Ast_identifier: {
			auto identifier = (AstIdentifier *)type;
			return get_size(identifier->definition->expression);
		}
		case Ast_unary_operator: {
			auto unop = (AstUnaryOperator *)type;
			switch (unop->operation) {
				case '*': return 8;
				default: invalid_code_path();
			}
		}
		case Ast_subscript: {
			auto subscript = (AstSubscript *)type;
			return get_align(subscript->expression);
		}
		case Ast_lambda: {
			return 8;
		}
		default: {
			invalid_code_path();
			return 0;
		}
	}
}

bool types_match_ns(AstExpression *a, AstExpression *b) {
	while (a->kind == Ast_identifier) {
		a = ((AstIdentifier *)a)->definition->expression;
	}
	while (b->kind == Ast_identifier) {
		b = ((AstIdentifier *)b)->definition->expression;
	}

	if (a->kind != b->kind) {
		return false;
	}

	if (a->kind == Ast_struct) {
		return a == b;
	}

	if (a->kind == Ast_unary_operator) {
		return types_match(((AstUnaryOperator *)a)->expression, ((AstUnaryOperator *)b)->expression);
	}

	auto eq = [](auto a, auto b) {
		if (!a.has_value()) return false;
		if (!b.has_value()) return false;
		return a.value() == b.value();
	};

	if (a->kind == Ast_subscript) {
		return types_match(((AstSubscript *)a)->expression, ((AstSubscript *)b)->expression) &&
			eq(get_constant_integer(((AstSubscript *)a)->index_expression),
			get_constant_integer(((AstSubscript *)b)->index_expression));
	}

	if (a->kind == Ast_lambda) {
		auto al = (AstLambda *)a;
		auto bl = (AstLambda *)b;
		if (al->parameters.count != bl->parameters.count)
			return false;

		if (!types_match(al->return_parameter->type, bl->return_parameter->type))
			return false;

		for (umm i = 0; i < al->parameters.count; ++i) {
			if (!types_match(al->parameters[i]->type, bl->parameters[i]->type))
				return false;
		}

		if (al->convention != bl->convention)
			return false;

		return true;
	}

	return false;
}

bool types_match(AstExpression *type_a, AstExpression *type_b) {
	if (type_a == type_b)
		return true;

	return types_match_ns(type_a, type_b) || types_match_ns(type_b, type_a);
}

AstExpression *direct(AstExpression *type) {
	switch (type->kind) {
		case Ast_identifier: {
			do {
				auto identifier = (AstIdentifier *)type;
				assert(identifier->definition);
				type = identifier->definition->expression;
			} while (type->kind == Ast_identifier);
			break;
		}
		case Ast_unary_operator:
		case Ast_subscript:
		case Ast_struct:
			break;
		default: invalid_code_path();
	}

	return type;
}

AstStruct *get_struct(AstExpression *type) {
	type = direct(type);
	if (type->kind == Ast_struct)
		return (AstStruct *)type;

	return 0;
}

struct AllocationBlock {
	u8 *base   = 0;
	u8 *cursor = 0;
	umm size   = 0;
};

List<AllocationBlock> ast_allocation_blocks;
u32 last_allocation_block_index;
Mutex allocation_mutex;

umm ast_allocation_block_size;

u32 debug_allocation_blocks = 0;

void new_ast_block() {
	AllocationBlock block;
	block.size = ast_allocation_block_size;
	block.cursor = block.base = os_allocator.allocate<u8>(ast_allocation_block_size);
	assert(block.cursor);
	ast_allocation_blocks.add(block);
	last_allocation_block_index += 1;
	atomic_increment(&debug_allocation_blocks);
}

void init_ast_allocator() {
	ast_allocation_blocks.allocator = os_allocator;
	ast_allocation_block_size = 1024*1024;
	last_allocation_block_index = -1;
	new_ast_block();
	ast_allocation_block_size *= 2;
}

void *my_allocate(umm size, umm align) {
	scoped_lock(allocation_mutex);

	if (ast_allocation_blocks.count == 0) {
		init_ast_allocator();
	}

retry:
	auto block = &ast_allocation_blocks[last_allocation_block_index];

	u8 *target = (u8 *)(((umm)block->cursor + align - 1) & ~(align - 1));
	if ((u8 *)block->base + block->size - target < size) {
		ast_allocation_block_size *= 2;
		while (ast_allocation_block_size < size + align - 1) {
			ast_allocation_block_size *= 2;
		}
		new_ast_block();
		goto retry;
	}

	block->cursor = target + size;

	return target;
}
void *my_reallocate(void *data, umm old_size, umm new_size, umm align) {
	auto result = my_allocate(new_size, align);
	memcpy(result, data, old_size);
	return result;
}

void my_deallocate(void *data, umm size) {

}

AstLambda *main_lambda;

Span<utf8> operator_string(u64 op) {
	switch (op) {
		case '+': return u8"+"s;
		case '-': return u8"-"s;
		case '*': return u8"*"s;
		case '/': return u8"/"s;
		case '%': return u8"%"s;
		case '|': return u8"|"s;
		case '&': return u8"&"s;
		case '^': return u8"^"s;
		case '.': return u8"."s;
		case '>': return u8">"s;
		case '<': return u8"<"s;
		case '>=': return u8">="s;
		case '<=': return u8"<="s;
		case '==': return u8"=="s;
		case '!=': return u8"!="s;
		case '=': return u8"="s;
		case '+=': return u8"+="s;
		case '-=': return u8"-="s;
		case '*=': return u8"*="s;
		case '/=': return u8"/="s;
		case '%=': return u8"%="s;
		case '|=': return u8"|="s;
		case '&=': return u8"&="s;
		case '^=': return u8"^="s;
		case '>>': return u8">>"s;
		case '<<': return u8"<<"s;
		case '>>=': return u8">>="s;
		case '<<=': return u8"<<="s;
	}
	invalid_code_path();
}
Span<utf8> operator_string(BinaryOperation op) {
	using enum BinaryOperation;
	switch (op) {
		case add: return u8"+"s;
		case sub: return u8"-"s;
		case mul: return u8"*"s;
		case div: return u8"/"s;
		case mod: return u8"%"s;
		case bor: return u8"|"s;
		case band: return u8"&"s;
		case bxor: return u8"^"s;
		case bsr: return u8">>"s;
		case bsl: return u8"<<"s;
		case dot: return u8"."s;
		case gt: return u8">"s;
		case lt: return u8"<"s;
		case ge: return u8">="s;
		case le: return u8"<="s;
		case eq: return u8"=="s;
		case ne: return u8"!="s;
		case ass: return u8"="s;
		case addass: return u8"+="s;
		case subass: return u8"-="s;
		case mulass: return u8"*="s;
		case divass: return u8"/="s;
		case modass: return u8"%="s;
		case borass: return u8"|="s;
		case bandass: return u8"&="s;
		case bxorass: return u8"^="s;
		case bsrass: return u8">>="s;
		case bslass: return u8"<<="s;
	}
	invalid_code_path();
}

// TODO: can types_match be replaced with ==   ???
bool is_integer(AstExpression *type) {
	return
		types_match(type, &type_unsized_integer) ||
		types_match(type, &type_u8) ||
		types_match(type, &type_u16) ||
		types_match(type, &type_u32) ||
		types_match(type, &type_u64) ||
		types_match(type, &type_s8) ||
		types_match(type, &type_s16) ||
		types_match(type, &type_s32) ||
		types_match(type, &type_s64);
}

// TODO: can types_match be replaced with ==   ???
bool is_signed(AstExpression *type) {
	return
		types_match(type, &type_s8) ||
		types_match(type, &type_s16) ||
		types_match(type, &type_s32) ||
		types_match(type, &type_s64);
}

#if OVERLOAD_NEW
void *operator new(umm size) {
	return my_allocate(size, __STDCPP_DEFAULT_NEW_ALIGNMENT__);
}
void *operator new(umm size, std::align_val_t align) {
	return my_allocate(size, (umm)align);
}
void operator delete(void *) {
}
void operator delete(void *data, umm size) {
	my_deallocate(data, size);
}
#endif

bool is_pointer(AstExpression *type) {
	if (type->kind == Ast_identifier) {
		return is_pointer(((AstIdentifier *)type)->definition->expression);
	}
	return type->kind == Ast_unary_operator && ((AstUnaryOperator *)type)->operation == '*';
}

AstLiteral *get_literal(AstExpression *expression) {
    switch (expression->kind) {
        case Ast_literal: return (AstLiteral *)expression;
        case Ast_identifier: {
            auto identifier = (AstIdentifier *)expression;
            auto definition = identifier->definition;
            if (definition && definition->is_constant) {
                return get_literal(definition->expression);
            }
            break;
        }
    }
    return 0;
}

bool is_constant(AstExpression *expression) {
    if (expression->kind == Ast_literal)
        return true;

    if (expression->kind == Ast_identifier) {
        auto identifier = (AstIdentifier *)expression;
        if (identifier->definition)
            return identifier->definition->is_constant;
        return false;
    }

    if (expression->kind == Ast_binary_operator) {
        auto binop = (AstBinaryOperator *)expression;
        return is_constant(binop->left) && is_constant(binop->right);
    }

    if (expression->kind == Ast_lambda) {
        return true;
    }

    if (is_type(expression))
        return true;

    return false;
}

AstLambda *get_lambda(AstExpression *expression) {
	switch (expression->kind) {
		case Ast_lambda:
			return (AstLambda *)expression;
		case Ast_identifier: {
			auto ident = (AstIdentifier *)expression;
			assert(ident->definition->expression);
			return get_lambda(ident->definition->expression);
		}
	}
	return 0;
}
