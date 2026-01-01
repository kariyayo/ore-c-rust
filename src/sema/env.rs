use std::collections::{HashMap, HashSet};

use crate::parser::ast::{FunctionDecl, StatementNode, StructDecl, StructRef, TypeRef};

mod types;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TypeDefinition {
    Basic(String),
    Struct(StructDecl),
    Typedef { alias: String, ty: Type }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Type {
    Basic(String),
    Struct(StructDecl),
    Pointer(Box<Type>),
    Array {
        type_dec: Box<Type>,
        size: Option<u32>,
    },
    Typedef { alias: String, ty: Box<Type> },
}

impl Type {
    pub fn type_name(&self) -> String {
        match self {
            Type::Basic(name) => name.to_string(),
            Type::Struct(struct_decl) => {
                if let Some(tag_name) = &struct_decl.tag_name {
                    // TODO: "struct <tag_name>" にしたい
                    tag_name.to_string()
                } else {
                    "struct".to_string()
                }
            }
            Type::Pointer(ty) => format!("{}*", ty.type_name()),
            Type::Array { type_dec, .. } => format!("{}[]", type_dec.type_name()),
            Type::Typedef { alias, .. } => format!("typedef struct {}", alias), // supported only struct
        }
    }
}

#[derive(Debug)]
pub struct Env {
    functions: Functions,

    /// scope_idx から Scope を取得する
    pub scopes: Vec<Scope>,

    /// ASTノードから scope_idx を取得するために利用する
    scope_table: HashMap<StatementNode, usize>
}

impl Env {
    pub fn new() -> Env {
        // let types = TypeTable { entities: generate_c_types() };
        // let global_scope = Scope { parent: None, names: HashSet::new(), types };
        Env {
            functions: Functions { entities: HashMap::new() },
            scopes: vec![],
            scope_table: HashMap::new(),
        }
    }

    pub fn create_global_scope(&mut self) -> LocalScope {
        if !self.scopes.is_empty() {
            panic!("global scope is already created")
        }
        let mut scope = LocalScope::new(0, 0);
        scope.types = TypeTable { entities: generate_c_types() };
        scope
    }

    pub fn create_local_scope(&mut self, parent: usize) -> LocalScope {
        if self.scopes.is_empty() {
            panic!("global scope is no exists")
        }
        LocalScope::new(self.scopes.len(), parent)
    }

    pub fn set_global_scope(&mut self, scope: Scope) {
        self.scopes.push(scope);
    }

    pub fn get_global_scope(&self) -> &Scope {
        self.scopes.get(0).unwrap()
    }

    pub fn put_scope(&mut self, node: StatementNode, scope: Scope) {
    // pub fn append_scope(&mut self, node: StatementNode, scope: Scope) {
        // println!("@@@@@@@@@@@@@@@@@@@ append_scope. scope: {:?}, node: {:?}", scope, node);
        let scope_id = scope.id;
        self.scopes.resize_with(scope_id+1, || Scope { id: 0, parent: None, vars: HashMap::new(), types: TypeTable { entities: HashMap::new() } });
        self.scopes[scope_id] = scope;
        // self.scopes.push(scope);
        self.scope_table.insert(node, scope_id);
        // self.scope_table.insert(node, scopes.len() - 1);
    }

    pub fn put_functions(&mut self, functions: Functions) {
        self.functions = functions
    }

    pub fn find_function(&self, name: &str) -> Option<FunctionDecl> {
        self.functions.find(name)
    }

    /// 変数名が定義されているかチェック
    pub fn is_defined(&self, local_scope: &LocalScope, name: &str) -> bool {
        println!("^^^^^^^^^^^ is_defined from local_scope ---> name: {}, local_scope: {:?}, parent_idx: {:?}", name, local_scope.vars, local_scope.parent);
        if let Some(ty_ref) = local_scope.vars.get(name) {
            println!("^^^^^^^^^^^ is_defined from scope: {}, OK!!", name);
            return true;
        };
        let mut scope_opt = self.scopes.get(local_scope.parent);

        // while let Some(scope) = scope_opt {
        //     // println!("^^^^^^^^^^^ is_defined from scope: {}, {:?}, parent_idx: {:?}", name, scope.names, scope.parent);
        //     if scope.find(name) {
        //         return true;
        //     }
        //     let Some(parent_idx) = scope.parent else {
        //         return false;
        //     };
        //     scope_opt = self.scopes.get(parent_idx);
        // }
        // false

        // 変数名 -> 型名の変換
        while let Some(scope) = scope_opt {
            println!("^^^^^^^^^^^ is_defined from scope: {}, {:?}, parent_idx: {:?}", name, scope.vars, scope.parent);
            if let Some(ty_ref) = scope.vars.get(name) {
                return true;
            };
            let Some(parent_idx) = scope.parent else {
                return false;
            };
            scope_opt = self.scopes.get(parent_idx);
        };
        false
    }

    /// 変数名 -> Type の解決
    pub fn solve_type_by_name(&self, scope: &Scope, name: &str) -> Option<Type> {
        let mut scope_opt = Some(scope);

        // 変数名 -> 型名の変換
        let mut type_ref_opt: Option<&TypeRef> = None;
        while let Some(scope) = scope_opt {
            if let Some(ty_ref) = scope.vars.get(name) {
                type_ref_opt = Some(ty_ref);
                break;
            };
            let Some(parent_idx) = scope.parent else {
                break;
            };
            scope_opt = self.scopes.get(parent_idx);
        };

        let Some(type_ref) = type_ref_opt else {
            return None
        };

        // 型名 -> Type の変換
        while let Some(scope) = scope_opt {
            let ty = self.solve_type(scope, type_ref);
            if ty.is_some() {
                return ty;
            }
            let Some(parent_idx) = scope.parent else {
                return None;
            };
            scope_opt = self.scopes.get(parent_idx);
        };
        None
    }

    /// TypeRef -> Type の解決
    pub fn solve_type_in_global_scope(&self, type_ref: &TypeRef) -> Option<Type> {
        self.get_global_scope().solve_type(type_ref)
    }

    /// TypeRef -> Type の解決
    pub fn solve_type(&self, scope: &Scope, type_ref: &TypeRef) -> Option<Type> {
        // println!("1111 $$$$$$$$$$$$$$$$$$$$ solve_type: {:?}, scope: {:?}", type_ref, scope);
        let mut scope_opt = Some(scope);
        while let Some(scope) = scope_opt {
            if let Some(ty) = scope.solve_type(type_ref) {
                return Some(ty);
            };
            let Some(parent_idx) = scope.parent else {
                return None;
            };
        // println!("$$$$$$$$$$$$$$$$$$$$ solve_type ---> parent_idx: {}", parent_idx);
            scope_opt = self.scopes.get(parent_idx);
        }
        None
    }

    /// TypeRef -> Type の解決
    pub fn solve_type_from_local_scope(&self, local_scope: &LocalScope, type_ref: &TypeRef) -> Option<Type> {
        println!("2222 $$$$$$$$$$$$$$$$$$$$ solve_type_from_local_scope: {:?}, local_scope: {:?}", type_ref, local_scope);
        if let Some(ty) = local_scope.solve_type(type_ref) {
            return Some(ty);
        };
        let Some(parent_scope) = self.scopes.get(local_scope.parent) else {
            return None;
        };
        self.solve_type(parent_scope, type_ref)
    }

    /// StatementNodeからScopeを取得
    pub fn scope_by_node(&self, node: &StatementNode) -> Option<&Scope> {
        self.scope_table.get(node).and_then(|idx| self.scopes.get(*idx))
    }

}

/// 型名と型の対応づけ
#[derive(Debug, Clone)]
struct TypeTable {
    /// 型名と型の対応づけ
    /// 
    /// ex)
    ///     - int -> TypeDefinition::Basic(int)
    ///     - struct point -> TypeDefinition::Struct(StructDecl)
    ///     - alias -> TypeDefinition::Typedef { alias: String, ty: Type }
    entities: HashMap<String, TypeDefinition>,
}

impl TypeTable {
    fn put(&mut self, name: &str) {
        // TODO: 重複チェック
        self.entities.insert(name.to_string(), TypeDefinition::Basic(name.to_string()));
    }

    fn put_struct_decl(&mut self, struct_decl: StructDecl) {
        // TODO: 重複チェック
        let key = format!("struct {}", struct_decl.clone().tag_name.unwrap_or_default());
        let ty = TypeDefinition::Struct(struct_decl);
        println!("%%%%%%%%%%%%%% key: {}", key);
        self.entities.insert(key, ty.clone());
    }

    fn put_typealias(&mut self, alias: &str, ty: Type) {
        // TODO: aliasの重複チェック
        self.entities.insert(alias.to_string(), TypeDefinition::Typedef { alias: alias.to_string(), ty });
    }

    fn find(&self, name: &str) -> Option<&TypeDefinition> {
        // println!("$$$$$$$$$$$$$$ name: {}, entities: {:?}", name, self.entities);
        self.entities.get(name)
    }

    fn find_struct_decl(&self, struct_decl: &StructDecl) -> Option<TypeDefinition> {
        let key = format!("{}", struct_decl.clone().tag_name.unwrap_or_default());
        if struct_decl.members.is_empty() {
            self.entities.get(&key).cloned()
        } else {
            let t = TypeDefinition::Struct(struct_decl.clone());
            Some(t)
        }
    }
}

#[derive(Debug)]
pub struct Functions {
    pub entities: HashMap<String, FunctionDecl>,
}

impl Functions {
    pub fn put(&mut self, function: FunctionDecl) {
        self.entities.insert(function.name.to_string(), function);
    }
    fn find(&self, name: &str) -> Option<FunctionDecl> {
        self.entities.get(name).cloned()
    }
}

/// 確定したScope
#[derive(Debug)]
pub struct Scope {
    pub id: usize,

    parent: Option<usize>,

    /// 変数名と型名の対応づけ
    vars: HashMap<String, TypeRef>,

    /// 型名と型の対応づけ
    types: TypeTable,
}

impl Scope {
    pub fn create_global_scope(global_scope: LocalScope) -> Scope {
        Scope {
            id: 0,
            parent: None,
            vars: global_scope.vars.clone(),
            types: global_scope.types.clone(),
        }
    }

    fn find(&self, name: &str) -> bool {
        // println!("^^^^^^^^^^^ scope.is_defined: {}, {:?}, parent_idx: {:?}", name, self.names, self.parent);
        self.vars.contains_key(name)
    }

    /// TypeRef -> Type の解決
    fn solve_type(&self, type_ref: &TypeRef) -> Option<Type> {
        // println!("$$$$$$$$$$$$$$$$$$$$ scope.solve_type: {:?}", type_ref);
        match type_ref {
            TypeRef::Named(name) => {
                // TODO: ここは TypeDefinition::Basic もしくは TypeDefinition::Typedef の場合がある
                self.types.find(name.as_str()).map(|ty| Type::Basic(name.to_string()))
            },
            TypeRef::Pointer(in_tyref) => self.solve_type(in_tyref).map(|ty| Type::Pointer(Box::new(ty))),
            TypeRef::Array { type_ref: in_tyref, size } => self.solve_type(in_tyref).map(|ty| Type::Array { type_dec: Box::new(ty), size: size.to_owned() }),
            TypeRef::Struct(struct_ref) => {
                match struct_ref {
                    StructRef::TagName(name) => {
                        // println!("@@@@@@@@@@@@@@@@@@@ name: {}, types: {:?}", name, self.types);
                        let n = format!("struct {}", name);
                        // println!("@@@@@@@@@@@@@@@@@@@ n: {}", n);
                        // TODO: ここは TypeDefinition::Struct以外はエラーにするべき
                        self.types.find(n.as_str()).map(|ty| Type::Basic(name.to_string()))
                    },
                    StructRef::Decl(struct_decl) => {
                        self.types.find_struct_decl(struct_decl).map(|_| Type::Struct(struct_decl.clone()))
                    },
                }
            }
            TypeRef::TypeAlias(in_tyref) => self.solve_type(in_tyref),
        }
    }

    /// typedefのalias -> Type の解決
    fn find_typedef(&self, name: &str) -> Option<&TypeDefinition> {
        self.types.find(name)
    }
}

/// 現在のScopeを表す。まだ要素が追加される可能性がある状態
#[derive(Debug)]
pub struct LocalScope {
    pub id: usize,

    parent: usize,

    /// 変数名と型名の対応づけ
    vars: HashMap<String, TypeRef>,

    /// 型名と型の対応づけ
    types: TypeTable,
}

impl LocalScope {

    pub fn new(id: usize, parent: usize) -> LocalScope {
        let types = TypeTable { entities: HashMap::new() };
        LocalScope {
            id,
            parent,
            vars: HashMap::new(),
            types,
        }
    }

    pub fn put_var(&mut self, name: &str, type_ref: &TypeRef) {
        println!("=>>> name: {}, type_ref: {:?}, parent_idx: {:?}", name, type_ref, self.parent);
        // TODO: 重複チェック
        self.vars.insert(name.to_string(), type_ref.clone());
    }

    fn is_defined(&self, name: &str) -> bool {
        // println!("^^^^^^^^^^^ local_scope.is_defined: {}, {:?}, parent_idx: {:?}", name, self.names, self.parent);
        self.vars.contains_key(name)
    }

    fn solve_type(&self, type_ref: &TypeRef) -> Option<Type> {
        // println!("$$$$$$$$$$$$$$$$$$$$ local_scope.solve_type: {:?}", type_ref);
        match type_ref {
            TypeRef::Named(name) => {
                self.types.find(name.as_str()).map(|_| Type::Basic(name.to_string()))
            },
            TypeRef::Pointer(in_tyref) => self.solve_type(in_tyref).map(|ty| Type::Pointer(Box::new(ty))),
            TypeRef::Array { type_ref: in_tyref, size } => self.solve_type(in_tyref).map(|ty| Type::Array { type_dec: Box::new(ty), size: size.to_owned() }),
            TypeRef::Struct(struct_ref) => {
                match struct_ref {
                    StructRef::TagName(name) => {
                        // TODO: ここは何を返すべきだ？
                        // println!("################## name: {:?}", name);
                        let n = format!("struct {}", name);
                        // println!("################## n: {}", n);
                        
                        self.types.find(n.as_str()).map(|a| {
                            // println!("################## a: {:?}", a);
                            Type::Basic(name.to_string())
                        })
                    },
                    StructRef::Decl(struct_decl) => {
                        self.types.find_struct_decl(struct_decl).map(|_| Type::Struct(struct_decl.clone()))
                    },
                }
            }
            TypeRef::TypeAlias(in_tyref) => self.solve_type(in_tyref),
        }
    }

    fn put_basic_type(&mut self, name: &str) {
        self.types.put(name);
    }

    pub fn put_struct_type(&mut self, struct_decl: StructDecl) {
        self.types.put_struct_decl(struct_decl)
    }

    // fn find_typedef(&self, name: &str) -> Option<&TypeDefinition> {
    //     let res = self.types.find(name);
    //     if res.is_some() {
    //         return res
    //     };

    //     let mut scope_opt = self.env.get_parent_scope(self);
    //     while let Some(scope) = scope_opt {
    //         let r = scope.find_typedef(name);
    //         if r.is_some() {
    //             return r;
    //         }
    //         scope_opt = scope.parent.and_then(|parent_idx| self.env.scopes.get(parent_idx));
    //     };
    //     None
    // }

    fn put_typealias(&mut self, env: &Env, aliases: Vec<String>, name: &str) {
        let Some(ty) = env.solve_type_from_local_scope(self, &TypeRef::Named(name.to_string())) else {
            // TODO: 存在しない型に対するaliasを登録しようとしているのでエラー
            panic!("")
        };
        for alias in aliases {
            self.types.put_typealias(alias.as_str(), ty.clone());
        }
    }

    fn put_typealias_with_struct_decl(&mut self, aliases: Vec<String>, struct_decl: &StructDecl) {
        // 自分自身Scopeにstruct定義を追加
        self.put_struct_type(struct_decl.clone());
        let ty = Type::Struct(struct_decl.clone());
        for alias in aliases {
            self.types.put_typealias(alias.as_str(), ty.clone());
        }
    }

    /// Scopeを確定する
    pub fn fix(&self) -> Scope {
        let scope = Scope {
            id: self.id,
            parent: Some(self.parent),
            vars: self.vars.clone(),
            types: self.types.clone(),
        };
        scope
        // // TODO: 外で呼ぶ？
        // self.env.append_scope(node, scope);
        // self.env
    }
}

pub fn put_typedef(env: &Env, scope: &mut LocalScope, type_ref: &TypeRef, items: &Vec<String>) {
    let TypeRef::TypeAlias(type_ref) = type_ref else {
        // TODO: ありえないのでpanic
        panic!("")
    };
    let ty = *type_ref.clone();
    match &ty {
        TypeRef::Named(name) => {
            // TODO: nameが示す型が存在するかチェックする
            // OKなら型エイリアスを追加
            scope.put_typealias(env, items.to_vec(), name)
        },
        TypeRef::Struct(struct_ref) => {
            match struct_ref {
                StructRef::TagName(name) => {
                    // TODO: nameが示す型が存在するかチェックする
                    // OKなら型エイリアスを追加
                    scope.put_typealias(env, items.to_vec(), name)
                },
                StructRef::Decl(struct_decl) => {
                    scope.put_typealias_with_struct_decl(items.to_vec(), struct_decl)
                },
            }
        }
        // TODO: サポートしてないのでpanicにする
        _ => panic!("not supported typedef other than `TypeRef::Named` and `TypeRef::Struct`.")
    }
}

fn generate_c_types() -> HashMap<String, TypeDefinition> {
    let mut types = HashMap::new();

    types.insert("void".to_string(), TypeDefinition::Basic("void".to_string()));
    types.insert("int".to_string(), TypeDefinition::Basic("int".to_string()));
    types.insert("char".to_string(), TypeDefinition::Basic("char".to_string()));

    // todo...

    types
}
