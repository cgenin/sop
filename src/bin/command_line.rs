use clap::{App, Arg, SubCommand};

pub enum TypeCommand {
    None,
    Json,
}

pub struct CommandLine {
    pub type_command: TypeCommand,
    pub file: Option<String>,
}

pub fn command_line()  {

}