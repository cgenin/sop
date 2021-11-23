use crate::data_types::{ColumnDefinition, DataType};
use crate::table::Table;
use std::fmt;
use nom::IResult;

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum CommitRowsOption {
    None,
    Delete,
    Preserve,
}

pub type RelationProperties = Vec<ColumnDefinition>;

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct CreateTable {
    global_temporary: bool,
    table: Table,
    relational_properties: RelationProperties,
    commit_rows: CommitRowsOption,
    // TODO
    physical_properties: bool,
    // TODO
    table_properties: bool,
}
