#include <ast.h>

umm append(StringBuilder &builder, AstKind kind) {
	switch (kind) {
#define e(name) case Ast_ ## name: return append(builder, u8#name##s);
		ENUMERATE_AST_KIND(e)
#undef e
	}
	return append(builder, "(unknown AstKind)");
}

u32 ast_node_uid_counter;

HashMap<u32, AstDefinition> built_in_definitions;
AstDefinition definition_unsized_integer;
AstDefinition definition_void;
AstDefinition *definition_default_integer;

HashMap<Span<utf8>, AstStatement *> global_statements;
RecursiveMutex global_statements_mutex;

bool needs_semicolon(AstNode *node) {
	return node->kind != Ast_lambda;
}

bool can_be_global(AstStatement *statement) {
	if (statement->kind == Ast_definition)
		return true;
	return false;
}

AstDefinition &get_built_in_definition_from_token(TokenKind t) {
	return built_in_definitions.get_or_insert(t & keyword_built_in_type_mask);
}
AstDefinition *find_built_in_definition_from_token(TokenKind t) {
	return built_in_definitions.find(t & keyword_built_in_type_mask).raw();
}
