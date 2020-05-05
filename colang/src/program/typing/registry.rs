use crate::program::typing::templates::{ProtoTypeParameter, TypeTemplate, TypeTemplateId};
use crate::program::typing::types::{Type, TypeId, TypeInstantiationStatus};
use crate::program::Field;
use crate::scope::TypeScope;
use crate::utils::graphs;
use petgraph::algo::toposort;
use petgraph::Graph;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// A singleton storage for all types present in the program.
pub struct TypeRegistry {
    /// All types used in the program have are stored in this collection, and can be accessed
    /// through their IDs.
    pub(in crate::program::typing) types: HashMap<TypeId, Rc<RefCell<Type>>>,

    /// Same as `types`, but for type templates.
    pub(in crate::program::typing) templates: HashMap<TypeTemplateId, Rc<RefCell<TypeTemplate>>>,
}

impl TypeRegistry {
    /// Initialize a new registry. There should be only one instance of a `TypeRegistry`
    /// present in a program.
    pub fn new() -> TypeRegistry {
        let mut registry = TypeRegistry {
            types: HashMap::new(),
            templates: HashMap::new(),
        };

        create_basic_type("void", TypeId::Void, &mut registry);
        create_basic_type("int", TypeId::Int, &mut registry);
        create_basic_type("bool", TypeId::Bool, &mut registry);
        create_basic_type("char", TypeId::Char, &mut registry);
        create_basic_type("string", TypeId::String, &mut registry);

        create_array_template(&mut registry);
        create_pointer_template(&mut registry);

        registry
    }

    /// Adds a newly created type to the registry.
    ///
    /// This has to be done for all types as soon as they are created unless there is a very good
    /// reason to not do so (e.g. the error type).
    pub fn register(&mut self, type_: Type) -> Rc<RefCell<Type>> {
        let id = type_.type_id.clone();
        let type_ = Rc::new(RefCell::new(type_));
        let existing = self.types.insert(id.clone(), Rc::clone(&type_));
        if existing.is_some() {
            panic!(
                "Attempt to create a duplicate type with the same type ID: {:?}",
                id
            )
        }
        type_
    }

    pub fn lookup(&self, type_id: &TypeId) -> &Rc<RefCell<Type>> {
        &self.types[type_id]
    }

    pub fn void(&self) -> &Rc<RefCell<Type>> {
        &self.types[&TypeId::Void]
    }

    pub fn int(&self) -> &Rc<RefCell<Type>> {
        &self.types[&TypeId::Int]
    }

    pub fn bool(&self) -> &Rc<RefCell<Type>> {
        &self.types[&TypeId::Bool]
    }

    pub fn char(&self) -> &Rc<RefCell<Type>> {
        &self.types[&TypeId::Char]
    }

    pub fn string(&self) -> &Rc<RefCell<Type>> {
        &self.types[&TypeId::String]
    }

    pub fn array(&self) -> &Rc<RefCell<TypeTemplate>> {
        &self.templates[&TypeTemplateId::Array]
    }

    pub fn array_of(&mut self, element_type: &Rc<RefCell<Type>>) -> Rc<RefCell<Type>> {
        TypeTemplate::instantiate(
            Rc::clone(self.array()),
            vec![Rc::clone(element_type)],
            self,
            None,
        )
        .unwrap()
    }

    pub fn pointer(&self) -> &Rc<RefCell<TypeTemplate>> {
        &self.templates[&TypeTemplateId::Pointer]
    }

    pub fn pointer_to(&mut self, target_type: &Rc<RefCell<Type>>) -> Rc<RefCell<Type>> {
        TypeTemplate::instantiate(
            Rc::clone(self.pointer()),
            vec![Rc::clone(target_type)],
            self,
            None,
        )
        .unwrap()
    }

    /// Basic types are non-template internal types.
    pub fn basic_types(&self) -> Vec<&Rc<RefCell<Type>>> {
        vec![
            self.void(),
            self.int(),
            self.bool(),
            self.char(),
            self.string(),
        ]
    }

    pub fn all_types(&self) -> impl Iterator<Item = &Rc<RefCell<Type>>> {
        self.types.values()
    }

    pub fn all_user_defined_types(&self) -> impl Iterator<Item = &Rc<RefCell<Type>>> {
        self.types
            .values()
            .filter(|type_| type_.borrow().is_user_defined())
    }

    /// Unregisters (hides) a previously registered type from the registry.
    /// This type will not appear in calls to `all_user_defined`, but there still might remain
    /// references to it from other types. If such references from types that have not been removed
    /// exist in a program, it should be considered invalid.
    pub fn remove_type(&mut self, type_: &Type) {
        self.types.remove(&type_.type_id).expect(&format!(
            "Cannot remove type `{}` from the program: it is not registered",
            type_.name,
        ));
    }

    /// Performs a reverse topological sort of the type graph containing all types where edges
    /// are fields.
    ///
    /// If there is no cycles in types through their fields, this method succeeds and returns
    /// an ordering of types where every type is encountered after all of its' fields' types.
    ///
    /// If there is a cycle, the fields causing it are returned as `Err`.
    pub fn all_types_sorted(&self) -> Result<Vec<Rc<RefCell<Type>>>, TypeCycleThroughFields> {
        let mut type_graph = Graph::<Rc<RefCell<Type>>, Rc<RefCell<Field>>>::new();

        // Indices are stable as long as no nodes are removed.
        let mut node_indices = HashMap::new();
        for (type_id, type_) in self.types.iter() {
            let index = type_graph.add_node(Rc::clone(&type_));
            node_indices.insert(type_id.clone(), index);
        }

        for type_ in self.types.values() {
            let type_ = type_.borrow();
            for field in &type_.fields {
                type_graph.add_edge(
                    node_indices[&type_.type_id],
                    node_indices[&field.borrow().type_.borrow().type_id],
                    Rc::clone(&field),
                );
            }
        }

        toposort(&type_graph, None)
            .map(|types| {
                types
                    .into_iter()
                    .rev()
                    .map(|node_index| Rc::clone(&type_graph[node_index]))
                    .collect()
            })
            .map_err(|cycle| {
                let node_index = cycle.node_id();
                let mut cycle = graphs::find_any_cycle(&type_graph, node_index);

                // Shift `cycle` so that the first type is user-defined.
                let first_user_defined = cycle
                    .iter()
                    .position(|node_index| type_graph[*node_index].borrow().is_user_defined())
                    .expect("Type cycle of internal types only");
                let mut shifted_cycle: Vec<_> = cycle.drain(first_user_defined..).collect();
                shifted_cycle.append(&mut cycle);
                let cycle = shifted_cycle;

                let anchor_type = Rc::clone(&type_graph[cycle[0]]);
                let fields = cycle
                    .iter()
                    .zip(cycle.iter().skip(1).chain(cycle.iter().take(1)))
                    .map(|(u, v)| {
                        Rc::clone(type_graph.edges_connecting(*u, *v).next().unwrap().weight())
                    })
                    .collect();

                TypeCycleThroughFields {
                    anchor_type,
                    fields,
                }
            })
    }
}

/// A type cycle encountered when traversing their fields.
pub struct TypeCycleThroughFields {
    /// Arbitrary type in the cycle.
    pub anchor_type: Rc<RefCell<Type>>,

    /// Field path causing a cycle. The first field belongs to `anchor_type`, the last field has
    /// type `anchor_type`.
    pub fields: Vec<Rc<RefCell<Field>>>,
}

fn create_basic_type(
    name: impl Into<String>,
    id: TypeId,
    registry: &mut TypeRegistry,
) -> Rc<RefCell<Type>> {
    registry.register(Type {
        name: name.into(),
        type_id: id,
        definition_site: None,
        instantiation_data: None,
        fields: Vec::new(),
        methods: Vec::new(),
        scope: TypeScope::new(),
        instantiation_status: TypeInstantiationStatus::DepsMayNeedInstantiation,
    })
}

fn create_array_template(registry: &mut TypeRegistry) -> Rc<RefCell<TypeTemplate>> {
    let type_parameters = vec![ProtoTypeParameter {
        name: "T".to_string(),
        definition_site: None,
    }];

    TypeTemplate::new(
        TypeTemplateId::Array,
        "<array>".to_string(),
        type_parameters,
        None,
        Some(Box::new(|type_args| {
            format!("[{}]", type_args[0].borrow().name)
        })),
        registry,
    )
}

fn create_pointer_template(registry: &mut TypeRegistry) -> Rc<RefCell<TypeTemplate>> {
    let type_parameters = vec![ProtoTypeParameter {
        name: "T".to_string(),
        definition_site: None,
    }];

    TypeTemplate::new(
        TypeTemplateId::Pointer,
        "<pointer>".to_string(),
        type_parameters,
        None,
        Some(Box::new(|type_args| {
            format!("&{}", type_args[0].borrow().name)
        })),
        registry,
    )
}
