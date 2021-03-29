#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VariableAttribute {
    Const, // Compile time constant, no storage will be allocated runtime
    ReadOnly, // Run-time constant - storage may be allocated
    Synthetic, // Compiler generated value
}