use crate::errors::CompilationError;
use crate::program::TypeCycleThroughFields;

pub fn type_cycle_through_fields(cycle: TypeCycleThroughFields) -> CompilationError {
    let anchor_type = cycle.anchor_type.borrow();
    let anchor_field = cycle.fields[0].borrow();

    CompilationError::new(
        "E0047",
        format!(
            "type `{}` is a part of a type cycle through fields",
            anchor_type.name,
        ),
    )
    .with_location(anchor_field.definition_site)
    .with_subtitle("field type starts a type cycle")
    .with_free_note({
        let type_cycle: Vec<_> = cycle
            .fields
            .iter()
            .map(|field| {
                let field = field.borrow();
                format!(
                    "`{}` (through field `{}`)",
                    field.type_.borrow().name,
                    field.name
                )
            })
            .collect();
        let type_cycle = type_cycle.join("\n -> ");
        format!(
            "Type dependency cycle:\n    `{}`\n -> {}",
            anchor_type.name, type_cycle
        )
    })
}
