//! Higher ast

#[cfg(test)]
mod tests;

use crate::{
    ParsingError,
    str_parser::ast as lower_ast,
    str_parser::lexer,
    str_parser::token::{Keyword, TokenKind},
};
use binary::GenericBinding;
use comp_base::AnyResult;
use comp_base::global::attrs::{
    ClassImplementationFlags, FieldAttr, FieldImplementationFlags, MethodAttr,
    MethodImplementationFlags, TypeAttr, TypeSpecificAttr, Visibility,
};
use comp_base::global::instruction::StringInstruction;
use comp_base::global::{Error, IndexMap, StringTypeReference};
use enumflags2::make_bitflags;

#[allow(unused)]
#[derive(Debug)]
pub struct File {
    pub asts: Vec<Ast>,
}

impl TryFrom<lower_ast::File> for File {
    type Error = Error;
    fn try_from(value: lower_ast::File) -> AnyResult<Self> {
        Ok(Self {
            asts: value
                .asts
                .into_iter()
                .map(TryFrom::try_from)
                .try_collect()?,
        })
    }
}

#[allow(unused)]
#[derive(Debug)]
pub enum Ast {
    TypeDef(TypeDef),
    SetAssemblyName(String),
}

impl TryFrom<lower_ast::Ast> for Ast {
    type Error = Error;
    fn try_from(value: lower_ast::Ast) -> AnyResult<Self> {
        match value {
            lower_ast::Ast::TypeDef(type_def) => type_def.try_into().map(Self::TypeDef),
            lower_ast::Ast::SetAssemblyName(ast) => Ok(Self::SetAssemblyName(ast.name.content)),
        }
    }
}

#[allow(unused)]
#[derive(Debug)]
pub enum TypeDef {
    Class(ClassDef),
}

impl TryFrom<lower_ast::TypeDef> for TypeDef {
    type Error = Error;
    fn try_from(value: lower_ast::TypeDef) -> AnyResult<Self> {
        match value {
            lower_ast::TypeDef::Class(class) => class.try_into().map(Self::Class),
        }
    }
}
#[allow(unused)]
#[derive(Debug)]
pub struct ClassDef {
    pub attr: TypeAttr,
    pub name: String,
    pub parent: Option<StringTypeReference>,
    pub generic_bindings: IndexMap<String, GenericBinding>,
    pub items: Vec<ClassItem>,
}

trait SetVis {
    fn set_vis(&mut self, vis: Visibility);
}

impl SetVis for TypeAttr {
    fn set_vis(&mut self, vis: Visibility) {
        let _: &mut Self = self.set_vis(vis);
    }
}

impl SetVis for FieldAttr {
    fn set_vis(&mut self, vis: Visibility) {
        let _: &mut Self = self.set_vis(vis);
    }
}

impl SetVis for MethodAttr {
    fn set_vis(&mut self, vis: Visibility) {
        let _: &mut Self = self.set_vis(vis);
    }
}

fn try_set_vis<T: SetVis>(attr: &mut T, vis: &str) -> bool {
    if Keyword::Public == vis {
        attr.set_vis(Visibility::Public);
    } else if Keyword::Private == vis {
        attr.set_vis(Visibility::Private);
    } else if Keyword::InAssembly == vis {
        attr.set_vis(Visibility::AssemblyOnly);
    } else {
        return false;
    }
    true
}

fn parse_class_attr(modifiers: impl Iterator<Item = String>) -> AnyResult<TypeAttr> {
    let mut attr = TypeAttr::new(
        Visibility::Private,
        TypeSpecificAttr::Class(make_bitflags!(ClassImplementationFlags::{})),
    );
    for modifier in modifiers {
        let modifier = &modifier;
        if !try_set_vis(&mut attr, modifier) {
            if "static" == modifier {
                attr.specific_mut()
                    .unwrap_class_mut()
                    .0
                    .insert(ClassImplementationFlags::Static);
            } else {
                return Err(ParsingError::UnknownModifier(modifier.to_owned()).into());
            }
        }
    }
    Ok(attr)
}

impl TryFrom<lower_ast::ClassDef> for ClassDef {
    type Error = Error;
    fn try_from(value: lower_ast::ClassDef) -> AnyResult<Self> {
        let attr = parse_class_attr(value.modifiers.into_iter().map(|x| x.content))?;
        let name = value.name.content.clone();
        let items = value
            .class_items
            .into_iter()
            .map(TryFrom::try_from)
            .try_collect()?;
        let generics = value.generics.map(|x| {
            x.1.into_iter()
                .map(|x| format!("@{}", x.name.content))
                .collect::<Vec<_>>()
        }).unwrap_or_default();
        let bindings = value
            .where_clauses
            .map(|x| x.clauses.into_iter().map(|x| {
                let mut i = x.0.bindings.into_iter();
                let parent = i.next().map(|x| x.content);
                let implemented_interfaces = i.map(|x| x.content).collect::<Vec<_>>();
                GenericBinding::new(implemented_interfaces, parent)
            }).collect::<Vec<_>>())
            .unwrap_or_default();
        Ok(Self {
            attr,
            name,
            parent: value.parent_with_colon.map(|x| x.1.content),
            generic_bindings: generics
                .into_iter()
                .zip(bindings)
                .collect(),
            items,
        })
    }
}

#[allow(unused)]
#[derive(Debug)]
pub enum ClassItem {
    Field(Field),
    Method(Method),
}

impl TryFrom<lower_ast::ClassItem> for ClassItem {
    type Error = Error;
    fn try_from(value: lower_ast::ClassItem) -> AnyResult<Self> {
        match value {
            lower_ast::ClassItem::Field(field) => field.try_into().map(Self::Field),
            lower_ast::ClassItem::Method(method) => method.try_into().map(Self::Method),
        }
    }
}

#[allow(unused)]
#[derive(Debug)]
pub struct Field {
    pub attr: FieldAttr,
    pub name: String,
    pub ty: StringTypeReference,
}

impl TryFrom<lower_ast::Field> for Field {
    type Error = Error;
    fn try_from(value: lower_ast::Field) -> AnyResult<Self> {
        let attr = {
            let mut attr = FieldAttr::new(
                Visibility::Private,
                make_bitflags!(FieldImplementationFlags::{}),
            );
            for modifier in &value.modifiers {
                let modifier = &modifier.content;
                if !try_set_vis(&mut attr, modifier) {
                    if Keyword::Static == modifier {
                        attr.impl_flags_mut()
                            .insert(FieldImplementationFlags::Static);
                    } else {
                        return Err(ParsingError::UnknownModifier(modifier.clone()).into());
                    }
                }
            }
            attr
        };
        let name = value.name.content.clone();
        let ty = value.ty.content.clone();
        Ok(Self { attr, name, ty })
    }
}

#[allow(unused)]
#[derive(Debug)]
pub struct Method {
    pub attr: MethodAttr,
    pub name: String,
    pub arguments: Vec<StringTypeReference>,
    pub ret_ty: StringTypeReference,
    pub generic_bindings: IndexMap<String, GenericBinding>,
    pub statements: Vec<StringInstruction>,
}

impl TryFrom<lower_ast::Method> for Method {
    type Error = Error;
    fn try_from(value: lower_ast::Method) -> AnyResult<Self> {
        let mut attr = {
            let mut attr = MethodAttr::new(
                Visibility::Private,
                make_bitflags!(MethodImplementationFlags::{}),
                0,
            );
            for modifier in &value.modifiers {
                if matches!(modifier.kind, TokenKind::Integer) {
                    unsafe {
                        attr.set_register_len(
                            lexer::parse_int(modifier.content.clone()).unwrap_unchecked(),
                        );
                    }
                    continue;
                }
                let modifier = &modifier.content;
                if !try_set_vis(&mut attr, modifier) {
                    if Keyword::Static == modifier {
                        attr.impl_flags_mut()
                            .insert(MethodImplementationFlags::Static);
                    } else {
                        return Err(ParsingError::UnknownModifier(modifier.clone()).into());
                    }
                }
            }
            attr
        };
        let name = format!(
            "{}({})",
            value.name.content,
            value
                .arguments
                .iter()
                .map(|x| x.content.string_name_repr().as_str().to_owned())
                .collect::<Vec<_>>()
                .join("|")
        );
        
        let arguments = value.arguments.into_iter().map(|x| x.content).collect();
        let register_len = {
            let s = value.register_len.content;
            if s.len() <= 2 {
                s.parse::<u64>()?
            } else {
                match &s[..2] {
                    "0x" => u64::from_str_radix(&s[2..], 16),
                    "0o" => u64::from_str_radix(&s[2..], 8),
                    "0b" => u64::from_str_radix(&s[2..], 2),
                    _ => s.parse::<u64>(),
                }?
            }
        };
        attr.set_register_len(register_len);
        let ret_ty = value.ret_ty.content;
        let generics = value.generics.map(|x| {
            x.1.into_iter()
                .map(|x| format!("@{}", x.name.content))
                .collect::<Vec<_>>()
        }).unwrap_or_default();
        let bindings = value
            .where_clauses
            .map(|x| x.clauses.into_iter().map(|x| {
                let mut i = x.0.bindings.into_iter();
                let parent = i.next().map(|x| x.content);
                let implemented_interfaces = i.map(|x| x.content).collect::<Vec<_>>();
                GenericBinding::new(implemented_interfaces, parent)
            }).collect::<Vec<_>>())
            .unwrap_or_default();
        let statements = value.statements.into_iter().map(|x| x.0.content).collect();
        Ok(Self {
            attr,
            name,
            arguments,
            ret_ty,
            generic_bindings: generics
                .into_iter()
                .zip(bindings)
                .collect(),
            statements,
        })
    }
}
