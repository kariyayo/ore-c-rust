use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};
use uuid::Uuid;

use crate::parser::ast::{FunctionDecl, StatementNode, StructDecl, StructRef, TypeRef};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Type {
    Basic(String),
    Struct(StructDecl),
    Pointer(Box<Type>),
    Array {
        type_dec: Box<Type>,
        size: Option<u32>,
    },
    Typedef {
        alias: String,
        type_dec: Box<Type>,
    },
}

impl Type {
    pub fn type_name(&self) -> String {
        match self {
            Type::Basic(name) => name.to_string(),
            Type::Struct(struct_decl) => {
                let tag_name = struct_decl.tag_name.to_owned().unwrap_or_default();
                let members = struct_decl
                    .members
                    .iter()
                    .map(|member| member.name.clone())
                    .collect::<Vec<_>>()
                    .join(", ");
                if members.is_empty() || !tag_name.is_empty() {
                    format!("struct {}", tag_name)
                } else {
                    format!("struct {{{}}}", members)
                }
            }
            Type::Pointer(ty) => format!("{}*", ty.type_name()),
            Type::Array { type_dec, .. } => format!("{}[]", type_dec.type_name()),
            Type::Typedef { alias, type_dec } => {
                format!("typedef {} {}", alias, type_dec.type_name())
            }
        }
    }
}

/// 型名と型定義の対応表
///
/// int -> int
/// struct point -> struct point { x: int, y: int }
/// typedef score -> int [10]
#[derive(Debug)]
struct TypeTable {
    entities: HashMap<String, Type>,
    types: HashSet<Type>,
}

impl TypeTable {
    fn new() -> TypeTable {
        let entities = generate_c_types();
        let mut types: HashSet<Type> = HashSet::new();
        for value in entities.values() {
            types.insert(value.clone());
        }
        TypeTable { entities, types }
    }

    fn put_struct(&mut self, ty: StructDecl) {
        let name = if let Some(tag_name) = &ty.tag_name {
            tag_name.to_string()
        } else {
            Uuid::new_v4().to_string()
        };
        self.entities.insert(name, Type::Struct(ty.clone()));
        self.types.insert(Type::Struct(ty.clone()));
    }

    fn put_typedef(&mut self, alias: &str, ty: Type) -> Result<(), String> {
        let ty = Type::Typedef {
            alias: alias.to_string(),
            type_dec: Box::new(ty),
        };
        if self.entities.contains_key(alias) {
            return Err(format!("alias is already defined. alias: {}", alias));
        };
        self.entities.insert(alias.to_string(), ty.clone());
        self.types.insert(ty.clone());
        Ok(())
    }

    fn find_basic(&self, ty: &str) -> Option<Type> {
        let ty = self.entities.get(ty);
        ty.and_then(|t| self.types.get(t)).cloned()
    }

    fn find_by_struct_ref(&self, struct_ref: StructRef) -> Option<Type> {
        match struct_ref {
            StructRef::TagName(name) => self.entities.get(&name.to_string()).cloned(),
            StructRef::Decl(struct_decl) => Some(Type::Struct(struct_decl)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Functions {
    entities: HashMap<String, FunctionDecl>,
}

impl Functions {
    pub fn new() -> Functions {
        Functions {
            entities: HashMap::new(),
        }
    }

    fn put(&mut self, function: FunctionDecl) {
        self.entities.insert(function.name.to_string(), function);
    }

    pub fn find(&self, name: &str) -> Option<FunctionDecl> {
        self.entities.get(name).cloned()
    }
}

#[derive(Debug, Clone)]
pub struct LocalScope {
    pub id: usize,

    // 親LocalScopeのid
    parent: Option<usize>,

    /// シンボル名と型名を対応表
    entities: HashMap<String, TypeRef>,
}

impl LocalScope {
    /// シンボル名と型名を登録する
    pub fn put(&mut self, env: &mut Env, name: &str, type_ref: TypeRef) {
        self.entities.insert(name.to_string(), type_ref);
        env.scopes.insert(self.id, self.clone());
    }

    /// シンボル名 -> 型名（TypeRef）
    pub fn find(&self, env: &Env, name: &str) -> Option<TypeRef> {
        let mut scope_opt = Some(self);
        while let Some(scope) = scope_opt {
            if let Some(type_ref) = scope.entities.get(name).cloned() {
                return Some(type_ref);
            };
            scope_opt = scope
                .parent
                .and_then(|parent_id| env.scopes.get(&parent_id))
        }
        None
    }

    /// シンボル名が存在しているかどうか
    pub fn is_defined(&self, name: &str) -> bool {
        self.entities.contains_key(name)
    }
}

#[derive(Debug)]
pub struct Env {
    /// 型名と型定義の対応表
    /// FIXME: TypeTableはLocalScopeが持つべきかも
    type_table: TypeTable,

    /// 関数定義
    pub functions: Functions,

    /// スコープIDとスコープの対応表
    scopes: HashMap<usize, LocalScope>,

    /// ASTノードとスコープIDの対応表
    block_scope_table: HashMap<StatementNode, usize>,
}

impl Env {
    pub fn new() -> Env {
        let type_table = TypeTable::new();
        let functions = Functions::new();
        let global_scope = LocalScope {
            id: 0,
            parent: None,
            entities: HashMap::new(),
        };
        let mut scopes = HashMap::new();
        scopes.insert(0, global_scope);
        Env {
            type_table,
            functions,
            scopes,
            block_scope_table: HashMap::new(),
        }
    }

    pub fn create_scope(&mut self, parent: LocalScope, node: &StatementNode) -> &mut Env {
        let parent_id = parent.id;
        let next_id = self.scopes.len();

        let new_scope = if let Some(sid) = self.block_scope_table.get(node) {
            let es = self.scopes.get(sid).map(|s| s.entities.clone());
            LocalScope {
                id: next_id,
                parent: Some(parent_id),
                entities: es.unwrap_or_default(),
            }
        } else {
            LocalScope {
                id: next_id,
                parent: Some(parent_id),
                entities: HashMap::new(),
            }
        };

        self.scopes.insert(next_id, new_scope);
        self.block_scope_table.insert(node.clone(), next_id);
        self
    }

    pub fn get_global_scope(&self) -> &LocalScope {
        self.scopes.get(&0).unwrap()
    }

    /// ASTノードからScopeを取り出す
    pub fn scope_by_node<'a>(
        &'a self,
        scope: &'a LocalScope,
        node: &StatementNode,
    ) -> &'a LocalScope {
        if let Some(scope_id) = self.block_scope_table.get(node) {
            self.scopes.get(scope_id).unwrap()
        } else {
            scope
        }
    }

    /// 型名を解決する（globalスコープ専用になってる）
    pub fn solve_type(&self, type_ref: &TypeRef) -> std::result::Result<Type, String> {
        match type_ref {
            TypeRef::Named(name) => self.type_table.find_basic(name).ok_or(format!(
                "variable type `{}` is not defined",
                type_ref.type_name()
            )),
            TypeRef::Pointer(type_ref) => self
                .solve_type(type_ref)
                .map(|ty| Type::Pointer(Box::new(ty))),
            TypeRef::Array { type_ref, size } => self.solve_type(type_ref).map(|ty| Type::Array {
                type_dec: Box::new(ty),
                size: *size,
            }),
            TypeRef::Struct(struct_ref) => self
                .type_table
                .find_by_struct_ref(struct_ref.clone())
                .ok_or(format!(
                    "struct type `{}` is not defined",
                    type_ref.type_name()
                )),
            TypeRef::Typedef(in_type_ref) => self.solve_type(in_type_ref),
        }
    }

    pub fn put_function(&mut self, function: FunctionDecl) {
        self.functions.put(function);
    }

    pub fn find_function(&self, name: &str) -> Option<FunctionDecl> {
        self.functions.find(name)
    }

    /// structの型定義を追加する（globalスコープ専用になってる）
    pub fn put_struct_type(&mut self, ty: StructDecl) {
        self.type_table.put_struct(ty);
    }

    /// typedefを追加する（globalスコープ専用になってる）
    pub fn put_typedef(&mut self, alias: &str, ty: Type) -> Result<(), String> {
        self.type_table.put_typedef(alias, ty)
    }

    /// シンボル名（変数名）をグローバルスコープに追加する
    pub fn put_vardecl_to_global(&mut self, name: &str, type_ref: TypeRef) -> LocalScope {
        let scope = self.scopes.get(&0).unwrap();
        let mut new_scope = scope.clone();
        new_scope.put(self, name, type_ref);
        self.scopes.insert(new_scope.id, new_scope.clone());
        new_scope
    }

    /// シンボル名（変数名）から型名（TypeRef）を取り出す
    pub fn find_vardecl(&self, scope: &LocalScope, name: &str) -> Option<TypeRef> {
        let mut scope_opt = Some(scope);
        while let Some(scope) = scope_opt {
            let res = scope.find(self, name);
            if res.is_some() {
                return res;
            }
            scope_opt = scope
                .parent
                .and_then(|parent_id| self.scopes.get(&parent_id))
        }
        None
    }
}

fn generate_c_types() -> HashMap<String, Type> {
    let mut types = HashMap::new();

    types.insert("int".to_string(), Type::Basic("int".to_string()));
    types.insert("char".to_string(), Type::Basic("char".to_string()));

    // todo...

    types
}
