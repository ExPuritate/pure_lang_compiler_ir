use crate::str_parser::token::{Keyword, SpecialChar};

pub mod ast;
pub mod lexer;
#[cfg(test)]
mod tests;
pub mod token;

peg::parser! {
    pub grammar file_parser() for str {
        pub rule file() -> ast::File
            = a:(ast()*) { ast::File::new(a) }

        rule ast() -> ast::Ast =
            t:type_def() { ast::Ast::TypeDef(t) } /
            set_assembly_name_ast:set_assembly_name()
            { ast::Ast::SetAssemblyName(set_assembly_name_ast) }


        rule set_assembly_name() -> ast::SetAssemblyNameAst =
        __ ch_exclamation_mark:#{lexer::special_char_matcher(SpecialChar::ExclamationMark)} __
        __ kw_assembly_name:#{lexer::keyword_matcher(Keyword::AssemblyName)} __
        __ ch_colon:#{lexer::special_char_matcher(SpecialChar::Colon)} __
        __ name:#{lexer::IDENTIFIER} __
        __ ch_semicolon:#{lexer::special_char_matcher(SpecialChar::Semicolon)} __
        {ast::SetAssemblyNameAst::new(
            ch_exclamation_mark,
            kw_assembly_name,
            ch_colon,
            name,
            ch_semicolon,
        )}

        rule type_def() -> ast::TypeDef
            = c:class_def() { ast::TypeDef::Class(c) }

        rule class_def() -> ast::ClassDef =
        __ kw_class:#{lexer::keyword_matcher(Keyword::Class)} __
        modifiers:class_modifiers()
        __ name:#{lexer::IDENTIFIER} __
        __ generics:generics()? __
        parent_with_colon:(
            __ colon:#{lexer::special_char_matcher(SpecialChar::Colon)} __
            __ parent:#{lexer::STRING_TYPE_REFERENCE} __
            { (colon, parent) }
        )?
        __ where_clauses:where_clauses()? __
        __ ch_brace_open0:#{lexer::special_char_matcher(SpecialChar::BraceOpen)} __
        __ items: class_item()* __
        __ ch_brace_close0: #{lexer::special_char_matcher(SpecialChar::BraceClose)} __

        {ast::ClassDef::new(
            kw_class,
            modifiers.0,
            modifiers.1,
            modifiers.2,
            name,
            generics,
            parent_with_colon,
            where_clauses,
            ch_brace_open0,
            items,
            ch_brace_close0,
        )}

        rule generics() -> (
            token::StringToken,
            token::Punctuated<ast::GenericDef, token::StringToken>,
            token::StringToken
        ) =
        open:#{lexer::special_char_matcher(SpecialChar::AngleBracketOpen)}
        inner:
        #{lexer::PUNCTUATED(lexer::GENERIC_DEF, lexer::special_char_matcher(SpecialChar::Comma))}
        close:#{lexer::special_char_matcher(SpecialChar::AngleBracketClose)}
        { (open, inner, close) }


        rule where_clauses() -> ast::WhereClauses =
        __ kw_where:#{lexer::keyword_matcher(Keyword::Where)} __
        __
        single_where_clauses:
        (
            clause:single_where_clause()
            semicolon:#{lexer::special_char_matcher(SpecialChar::Semicolon)}
            {(clause, semicolon)}
        )*
        __ {
            ast::WhereClauses::new(
                kw_where,
                single_where_clauses,
            )
        }

        rule single_where_clause() -> ast::SingleWhereClause =
        __ name:#{lexer::IDENTIFIER} __
        __ ch_colon:#{lexer::special_char_matcher(SpecialChar::Colon)} __
        __ bindings:#{
            lexer::PUNCTUATED(
                lexer::STRING_TYPE_REFERENCE,
                lexer::special_char_matcher(SpecialChar::Comma)
            )
        } __ {
            ast::SingleWhereClause::new(
                name,
                ch_colon,
                bindings,
            )
        }

        rule class_item() -> ast::ClassItem =
        field:field() { ast::ClassItem::Field(field) } /
        method:method() { ast::ClassItem::Method(method) }

        rule field() -> ast::Field =
        __ kw_field:#{lexer::keyword_matcher(Keyword::Field)} __
        modifiers:field_modifiers()
        __ name:#{lexer::IDENTIFIER} __
        __ ch_colon:#{lexer::special_char_matcher(SpecialChar::Colon)} __
        __ ty:#{lexer::STRING_TYPE_REFERENCE} __
        __ ch_semicolon:#{lexer::special_char_matcher(SpecialChar::Semicolon)} __

        {ast::Field::new(
            kw_field,
            modifiers.0,
            modifiers.1,
            modifiers.2,
            name,
            ch_colon,
            ty,
            ch_semicolon,
        )}

        rule method() -> ast::Method =
        __ kw_method:#{lexer::keyword_matcher(Keyword::Method)} __
        modifiers:method_modifiers()
        __ name:#{lexer::IDENTIFIER} __
        __ generics:generics()? __

        __ ch_parenthesis_open0:#{lexer::special_char_matcher(SpecialChar::ParenthesisOpen)} __
        __ arguments:#{lexer::PUNCTUATED(lexer::STRING_TYPE_REFERENCE, lexer::special_char_matcher(SpecialChar::Comma))} __
        __ ch_parenthesis_close0:#{lexer::special_char_matcher(SpecialChar::ParenthesisClose)} __

        __ register_len:#{lexer::INTEGER_LITERAL} __

        __ ch_arrow:#{lexer::special_char_matcher(SpecialChar::Arrow)} __
        __ ret_ty:#{lexer::STRING_TYPE_REFERENCE} __

        __ where_clauses:where_clauses()? __

        __ ch_brace_open0:#{lexer::special_char_matcher(SpecialChar::BraceOpen)} __
        __ statements:#{lexer::METHOD_STATEMENT}* __
        __ ch_brace_close0: #{lexer::special_char_matcher(SpecialChar::BraceClose)} __

        {ast::Method::new(
            kw_method,
            modifiers.0,
            modifiers.1,
            modifiers.2,
            name,

            generics,

            ch_parenthesis_open0,
            arguments,
            ch_parenthesis_close0,

            register_len,

            ch_arrow,
            ret_ty,

            where_clauses,

            ch_brace_open0,
            statements,
            ch_brace_close0,
        )}

        /// Returns (bracket_open, modifiers, bracket_close)
        #[no_trace]
        rule field_modifiers() -> (token::StringToken, Vec<token::StringToken>, token::StringToken) =
        __ ch_bracket_open0:#{lexer::special_char_matcher(SpecialChar::BracketOpen)} __
        modifiers:(__ m:#{lexer::field_modifier_matcher()} __ { m })*
        __ ch_bracket_close0:#{lexer::special_char_matcher(SpecialChar::BracketClose)} __
        {
            (ch_bracket_open0, modifiers, ch_bracket_close0)
        }
        #[no_trace]
        rule method_modifiers() -> (token::StringToken, Vec<token::StringToken>, token::StringToken) =
        __ ch_bracket_open0:#{lexer::special_char_matcher(SpecialChar::BracketOpen)} __
        modifiers:(__ m:#{lexer::method_modifier_matcher()} __ { m })*
        __ ch_bracket_close0:#{lexer::special_char_matcher(SpecialChar::BracketClose)} __
        {
            (ch_bracket_open0, modifiers, ch_bracket_close0)
        }
        #[no_trace]
        rule class_modifiers() -> (token::StringToken, Vec<token::StringToken>, token::StringToken) =
        __ ch_bracket_open0:#{lexer::special_char_matcher(SpecialChar::BracketOpen)} __
        modifiers:(__ m:#{lexer::class_modifier_matcher()} __ { m })*
        __ ch_bracket_close0:#{lexer::special_char_matcher(SpecialChar::BracketClose)} __
        {
            (ch_bracket_open0, modifiers, ch_bracket_close0)
        }

        #[no_trace]
        rule _ -> ()
            = #{lexer::IGNORE(lexer::COMMENT())} / #{lexer::WHITESPACE}

        #[no_trace]
        rule __ -> () = _* {}
    }
}
