use gimli::{Dwarf, EndianSlice, EntriesTreeNode, LittleEndian};

use super::WasmError;


pub fn parse_dwarf_entries<'a>(dwarf: &Dwarf<EndianSlice<'a, LittleEndian>>) -> Result<(), WasmError> {
    let debug_abbrev = &dwarf.debug_abbrev;
    let mut units = dwarf.units();
    while let Some(unit_header) = units.next()? {
        let abbreviations = unit_header.abbreviations(debug_abbrev)?;
        let mut entries = unit_header.entries_tree(&abbreviations, None)?;
        let root = entries.root()?;
        parse_dwarf_entry(dwarf, root, "")?;
    }
    Ok(())
}

fn parse_dwarf_entry<'a, 'abbrev, 'unit, 'tree>(
    dwarf: &Dwarf<EndianSlice<'a, LittleEndian>>,
    tree: EntriesTreeNode<'abbrev, 'unit, 'tree, EndianSlice<'a, LittleEndian>>,
    indent: &str,
) -> Result<(), WasmError> {
    {
        let entry = tree.entry();
        println!("{indent}[{}]", entry.tag());
        let mut attrs = entry.attrs();
        while let Some(attr) = attrs.next()? {
            if let Some(string) = attr.string_value(&dwarf.debug_str) {
                println!("{indent}  {} = {:?}", attr.name(), string.to_string_lossy().to_string());
            } else if let Some(value) = attr.udata_value() {
                println!("{indent}  {} = {:?}", attr.name(), value);
            } else {
                println!("{indent}  {} = {:?}", attr.name(), attr.value());
            }
        }
    }
    let mut children = tree.children();
    while let Some(child) = children.next()? {
        parse_dwarf_entry(dwarf, child, &format!("{indent}  "))?;
    }

  Ok(())
}

pub struct DebuggingInformationEntry {
    
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum Tag {
    AccessDeclaration,
    ArrayType,
    AtomicType,
    BaseType,
    CallSite,
    CallSiteParameter,
    CatchBlock,
    ClassType,
    CoarrayType,
    CommonBlock,
    CommonInclusion,
    CompileUnit,
    Condition,
    ConstType,
    Constant,
    DwarfProcedure,
    DynamicType,
    EntryPoint,
    EnumerationType,
    Enumerator,
    FileType,
    FormalParameter,
    Friend,
    GenericSubrange,
    ImmutableType,
    ImportedDeclaration,
    ImportedModule,
    ImportedUnit,
    Inheritance,
    InlinedSubroutine,
    InterfaceType,
    Label,
    LexicalBlock,
    Member,
    Module,
    Namelist,
    NamelistItem,
    Namespace,
    PackedType,
    PartialUnit,
    PointerType,
    PtrToMemberType,
    ReferenceType,
    RestrictType,
    RvalueReferenceType,
    SetType,
    SharedType,
    SkeletonUnit,
    StringType,
    StructureType,
    Subprogram,
    SubrangeType,
    SubroutineType,
    TemplateAlias,
    TemplateTypeParameter,
    TemplateValueParameter,
    ThrownType,
    TryBlock,
    Typedef,
    TypeUnit,
    UnionType,
    UnspecifiedParameters,
    UnspecifiedType,
    Variable,
    Variant,
    VariantPart,
    VolatileType,
    WithStmt,
    Unknown(String),
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum AttributeType {
    AbstractOrigin,
    Accessibility,
    AddressClass,
    AddrBase,
    Alignment,
    Allocated,
    Artificial,
    Associated,
    BaseTypes,
    BinaryScale,
    BitSize,
    BitStride,
    ByteSize,
    ByteStride,
    CallAllCalls,
    CallAllSourceCalls,
    CallAllTailCalls,
    CallColumn,
    CallDataLocation,
    CallDataValue,
    CallFile,
    CallLine,
    CallOrigin,
    CallParameter,
    CallPc,
    CallReturnPc,
    CallTailCall,
    CallTarget,
    CallTargetClobbered,
    CallValue,
    CallingConvention,
    CommonReference,
    CompDir,
    ConstExpr,
    ConstValue,
    ContainingType,
    Count,
    DataBitOffset,
    DataLocation,
    DataMemberLocation,
    DecimalScale,
    DecimalSign,
    DeclColumn,
    DeclFile,
    DeclLine,
    Declaration,
    Defaulted,
    DefaultValue,
    Deleted,
    Description,
    DigitCount,
    Discr,
    DiscrList,
    DiscrValue,
    DwoName,
    Elemental,
    Encoding,
    Edianity,
    EntryPc,
    EnumClass,
    Explicit,
    ExportSymbols,
    Extension,
    External,
    FrameBase,
    Friend,
    HighPc,
    IdentifierCase,
    Import,
    Inline,
    IsOptional,
    Language,
    LinkageName,
    Location,
    LoclistsBase,
    LowPc,
    LowerBound,
    MacroInfo,
    Macros,
    MainSubprogram,
    Mutable,
    Name,
    NamelistItem,
    Noreturn,
    ObjectPointer,
    Ordering,
    PictureString,
    Priority,
    Producer,
    Prototyped,
    Pure,
    Ranges,
    Rank,
    Recursive,
    Reference,
    ReturnAddr,
    RnglistsBase,
    RvalueReference,
    Segment,
    Sibling,
    Small,
    Signature,
    Specification,
    StartScope,
    StaticLink,
    StmtList,
    StringLength,
    StringLengthBitSize,
    StringLengthByteSize,
    StrOffsetsBase,
    ThreadsScaled,
    Trampoline,
    Type,
    UpperBound,
    UseLocation,
    UseUtf8,
    VariableParameter,
    Virtuality,
    VtableElemLocation,
    Unknown(String),
}


