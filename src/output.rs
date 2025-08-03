use crate::ast::{Ast, ClassItem, Field, File, Method, TypeDef};
use binary::Assembly;
use comp_base::AnyResult;
use comp_base::global::{IndexMap, StringName};
use std::io::Write;
use std::mem::MaybeUninit;

pub struct Output {
    pub(crate) file: File,
}

impl Output {
    #[allow(unused)]
    pub fn new(file: File) -> Self {
        Self { file }
    }
    pub fn out<W: Write>(&self, writer: &mut W) -> AnyResult<()> {
        let mut assembly = Assembly::default();
        let mut type_defs = MaybeUninit::new(assembly.type_defs_mut());
        fn map_class_item(
            i: &ClassItem,
            methods: &mut IndexMap<StringName, binary::method::Method>,
            fields: &mut IndexMap<StringName, binary::class::Field>,
        ) -> AnyResult<()> {
            match i {
                ClassItem::Field(field) => {
                    fields.insert(field.name.as_str().into(), map_field(field)?);
                    Ok(())
                }
                ClassItem::Method(method) => {
                    methods.insert(method.name.as_str().into(), map_method(method)?);
                    Ok(())
                }
            }
        }
        fn map_method(m: &Method) -> AnyResult<binary::method::Method> {
            Ok(binary::method::Method::new(
                m.name.as_str().into(),
                m.attr,
                m.statements.clone(),
                m.ret_ty.clone(),
                m.arguments.clone(),
                m.generic_bindings
                    .clone()
                    .into_iter()
                    .map(|(k, v)| (k.into(), v))
                    .collect(),
            ))
        }
        fn map_field(f: &Field) -> AnyResult<binary::field::Field> {
            Ok(binary::field::Field::new(
                f.name.as_str().into(),
                f.attr,
                f.ty.clone(),
            ))
        }
        for ast in &self.file.asts {
            match ast {
                Ast::TypeDef(TypeDef::Class(c)) => {
                    let mut methods = IndexMap::new();
                    let mut fields = IndexMap::new();
                    for item in &c.items {
                        map_class_item(item, &mut methods, &mut fields)?;
                    }
                    let class = binary::class::ClassDef::new(
                        c.parent.clone(),
                        c.generic_bindings
                            .clone()
                            .into_iter()
                            .map(|(k, v)| (k.into(), v))
                            .collect(),
                        c.attr,
                        c.name.as_str().into(),
                        methods,
                        fields,
                    );
                    unsafe {
                        type_defs
                            .assume_init_read()
                            .insert(c.name.as_str().into(), binary::TypeDef::Class(class));
                    }
                }
                Ast::SetAssemblyName(name) => {
                    #[allow(unused_assignments)]
                    type_defs = MaybeUninit::uninit();
                    *assembly.name_mut() = StringName::from(name.as_str());
                    type_defs = MaybeUninit::new(assembly.type_defs_mut());
                }
            }
        }
        writer.write_all(&assembly.to_file_bytes()?)?;
        Ok(())
    }
}
