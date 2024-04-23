//! Abstract syntax tree
//!
//! Types in this module represent various pieces a C program can
//! contain after preprocessing phase. They mostly follow C11 grammar
//! naming conventions.
//!
//! References to C11 standard given in parenthesis refer to the
//! [ISO/IEC 9899:201x
//! draft](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf)
//! published on April 12, 2011.
//!
//! A number of GNU extensions to the standard C are included here.
//! Types, struct fields or enum variants specific to GNU are marked as
//! "GNU extension" with a link to the relevant section of gcc
//! documentation. Supported extensions are:
//!
//! - attributes in various positions
//! - inline assembly statements and asm labels
//! - extensions to the initializer list syntax
//! - statement expressions
//! - `typeof` type specifiers

use ast;

// From 6.4 Lexical elements

/// Variable, function and other names that are not type names
///
/// (C11 6.4.2)
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Identifier {
    pub name: String,
}

/// Constant literals
///
/// C11 places string literals under primary expressions, thus they
/// are not included here.
///
/// (C11 6.4.4)
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Constant {
    Integer(Integer),
    Float(Float),
    Character(String),
}

/// Integer number literal
///
/// (C11 6.4.4.1)
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Integer {
    pub base: IntegerBase,
    pub number: Box<str>,
    pub suffix: IntegerSuffix,
}

/// Base of the integer literal
///
/// (C11 6.4.4.1)
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum IntegerBase {
    Decimal,
    Octal,
    Hexadecimal,
    /// [GNU extension](https://gcc.gnu.org/onlinedocs/gcc/Binary-constants.html)
    Binary,
}

/// Suffix of an integer literal
///
/// (C11 6.4.4.1)
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct IntegerSuffix {
    /// Minimum size of the integer literal
    pub size: IntegerSize,
    /// Integer literal has unsigned type
    pub unsigned: bool,
    /// Integer literal is an imaginary part of a complex number
    ///
    /// [GNU extension](https://gcc.gnu.org/onlinedocs/gcc/Complex.html) suffixes `i` and `j`.
    pub imaginary: bool,
}

/// Size part of a integer literal suffix
///
/// (C11 6.4.4.1)
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum IntegerSize {
    /// no `l` or `ll`
    Int = 0,
    /// `l`
    Long,
    /// `ll`
    LongLong,
}

/// Floating point number literal
///
/// (C11 6.4.4.2)
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Float {
    pub base: FloatBase,
    pub number: Box<str>,
    pub suffix: FloatSuffix,
}

/// Floating point number base
///
/// (C11 6.4.4.2)
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum FloatBase {
    Decimal,
    Hexadecimal,
}

/// Floating point number suffix
///
/// (C11 6.4.4.2)
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct FloatSuffix {
    pub format: FloatFormat,
    /// Integer literal is an imaginary part of a complex number
    ///
    /// [GNU extension](https://gcc.gnu.org/onlinedocs/gcc/Complex.html) suffixes `i` and `j`.
    pub imaginary: bool,
}

/// Floating point literal format specified by the suffix
///
/// (C11 6.4.4.2)
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum FloatFormat {
    /// `f` suffix
    Float,
    /// no suffix
    Double,
    /// `l` suffix
    LongDouble,
    /// [ISO/IEC TS 18661-2:2015](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1912.pdf)
    /// `df`, `dd`, `dl` suffixes
    ///
    /// [ISO/IEC TS 18661-3:2015](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1945.pdf)
    /// `fN`, `fNx`, `dN`, `dNx` suffixes
    TS18661Format(TS18661FloatType),
}

/// String literal
///
/// (C11 6.4.5)
pub type StringLiteral = Vec<String>;

// From 6.5 Expressions

/// Expressions
///
/// (C11 6.5)
#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    /// Identifier
    ///
    /// May be a variable, function name or enumerator. The latter is
    /// different from the standard, where enumerators are classified
    /// as constants.
    ///
    /// (C11 6.5.1)
    Identifier(Box<Identifier>),
    /// Numeric and character constants
    ///
    /// Enumerator constants, being valid identifiers, are reprented
    /// as `Identifier` in this enum.
    ///
    /// (C11 6.5.1)
    Constant(Box<Constant>),

    /// String literal
    ///
    /// (C11 6.5.1)
    StringLiteral(Box<StringLiteral>),

    /// Generic selection
    ///
    /// (C11 6.5.1.1)
    GenericSelection(Box<GenericSelection>),

    /// Structure and union members
    ///
    /// Both direct (`.`) and indirect (`->`) access.
    ///
    /// (C11 6.5.2)
    Member(Box<MemberExpression>),

    /// Function call expression
    ///
    /// (C11 6.5.2)
    Call(Box<CallExpression>),

    /// Compound literal
    ///
    /// (C11 6.5.2)
    CompoundLiteral(Box<CompoundLiteral>),

    /// Size of a type
    ///
    /// (C11 6.5.3)
    SizeOfTy(Box<SizeOfTy>),

    /// Size of a unary expression
    ///
    /// (C11 6.5.3)
    SizeOfVal(Box<SizeOfVal>),

    /// Alignment of a type
    ///
    /// (C11 6.5.3)
    AlignOf(Box<AlignOf>),

    /// Unary operators
    ///
    /// This represents both postfix and prefix unary oprators. Postfix expressions that take
    /// additional operands are represented by a separate entry in this enum.
    ///
    /// (C11 6.5.2, c11 6.5.3)
    UnaryOperator(Box<UnaryOperatorExpression>),

    /// Cast expression
    ///
    /// `(type) expr`
    ///
    /// (C11 6.5.4)
    Cast(Box<CastExpression>),

    /// Binary operators
    ///
    /// All of C binary operators that can be applied to two expressions.
    ///
    /// (C11 6.5.5 -- 6.5.16)
    BinaryOperator(Box<BinaryOperatorExpression>),

    /// Conditional operator
    ///
    /// (C11 6.5.15)
    Conditional(Box<ConditionalExpression>),

    /// Comma operator
    ///
    /// (C11 6.5.17)
    Comma(Box<Vec<Expression>>),

    /// Member offset expression
    ///
    /// Result of expansion of `offsetof` macro.
    ///
    /// (C11 7.19 §3).
    OffsetOf(Box<OffsetOfExpression>),

    /// Variable argument list access
    ///
    /// Result of expansion of `va_arg` macro.
    ///
    /// (C11 7.16.1.1).
    VaArg(Box<VaArgExpression>),

    /// Statement expression
    ///
    /// [GNU extension](https://gcc.gnu.org/onlinedocs/gcc/Statement-Exprs.html)
    Statement(Box<Statement>),
}

/// Struct or union member access
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum MemberOperator {
    /// `expression.identifier`
    Direct,
    /// `expression->identifier`
    Indirect,
}

/// Generic selection expression
///
/// (C11 6.5.1.1)
#[derive(Debug, PartialEq, Clone)]
pub struct GenericSelection {
    pub expression: Box<Expression>,
    pub associations: Vec<GenericAssociation>,
}

/// Single element of a generic selection expression
///
/// (C11 6.5.1.1)
#[derive(Debug, PartialEq, Clone)]
pub enum GenericAssociation {
    Type(GenericAssociationType),
    Default(Box<Expression>),
}

/// Type match case in a generic selection expression
///
/// (C11 6.5.1.1)
#[derive(Debug, PartialEq, Clone)]
pub struct GenericAssociationType {
    pub type_name: TypeName,
    pub expression: Box<Expression>,
}

/// Structure and union members
///
/// Both direct (`.`) and indirect (`->`) access.
///
/// (C11 6.5.2)
#[derive(Debug, PartialEq, Clone)]
pub struct MemberExpression {
    pub operator: MemberOperator,
    pub expression: Box<Expression>,
    pub identifier: Identifier,
}

/// Function call expression
///
/// (C11 6.5.2)
#[derive(Debug, PartialEq, Clone)]
pub struct CallExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
}

/// Compound literal
///
/// (C11 6.5.2)
#[derive(Debug, PartialEq, Clone)]
pub struct CompoundLiteral {
    pub type_name: TypeName,
    pub initializer_list: Vec<InitializerListItem>,
}

/// SizeOf a type
///
/// (C11 6.5.3)
#[derive(Debug, PartialEq, Clone)]
pub struct SizeOfTy(pub TypeName);

/// Size of an unary expression
///
/// (C11 6.5.3)
#[derive(Debug, PartialEq, Clone)]
pub struct SizeOfVal(pub Box<Expression>);

/// Alignment of a type
///
/// (C11 6.5.3)
#[derive(Debug, PartialEq, Clone)]
pub struct AlignOf(pub Box<TypeName>);

/// All operators with one operand
///
/// (C11 6.5)
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum UnaryOperator {
    /// `operand++`
    PostIncrement,
    /// `operand--`
    PostDecrement,
    /// `++operand`
    PreIncrement,
    /// `--operand`
    PreDecrement,
    /// `&operand`
    Address,
    /// `*operand`
    Indirection,
    /// `+operand`
    Plus,
    /// `-operand`
    Minus,
    /// `~operand`
    Complement,
    /// `!operand`
    Negate,
}

/// Unary operator expression
///
/// This represents both postfix and prefix unary oprators. Postfix expressions that take
/// additional operands are represented by a separate entry in this enum.
///
/// (C11 6.5.2, c11 6.5.3)
#[derive(Debug, PartialEq, Clone)]
pub struct UnaryOperatorExpression {
    pub operator: UnaryOperator,
    pub operand: Box<Expression>,
}

/// Cast expression
///
/// `(type) expr`
///
/// (C11 6.5.4)
#[derive(Debug, PartialEq, Clone)]
pub struct CastExpression {
    pub type_name: TypeName,
    pub expression: Box<Expression>,
}

/// All operators with two operands
///
/// (C11 6.5)
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum BinaryOperator {
    /// `lhs[rhs]`
    Index,
    /// `lhs * rhs`
    Multiply,
    /// `lhs / rhs`
    Divide,
    /// `lhs % rhs`
    Modulo,
    /// `lhs + rhs`
    Plus,
    /// `lhs - rhs`
    Minus,
    /// `lhs << rhs`
    ShiftLeft,
    /// `lhs >> rhs`
    ShiftRight,
    /// `lhs < rhs`
    Less,
    /// `lhs > rhs`
    Greater,
    /// `lhs <= rhs`
    LessOrEqual,
    /// `lhs >= rhs`
    GreaterOrEqual,
    /// `lhs == rhs`
    Equals,
    /// `lhs != rhs`
    NotEquals,
    /// `lhs & rhs`
    BitwiseAnd,
    /// `lhs ^ rhs`
    BitwiseXor,
    /// `lhs | rhs`
    BitwiseOr,
    /// `lhs && rhs`
    LogicalAnd,
    /// `lhs || rhs`
    LogicalOr,
    /// `lhs = rhs`
    Assign,
    /// `lhs *= rhs`
    AssignMultiply,
    /// `lhs /= rhs`
    AssignDivide,
    /// `lhs %= rhs`
    AssignModulo,
    /// `lhs += rhs`
    AssignPlus,
    /// `lhs -= rhs`
    AssignMinus,
    /// `lhs <<= rhs`
    AssignShiftLeft,
    /// `lhs >>= rhs`
    AssignShiftRight,
    /// `lhs &= rhs`
    AssignBitwiseAnd,
    /// `lhs ^= rhs`
    AssignBitwiseXor,
    /// `lhs |= rhs`
    AssignBitwiseOr,
}

/// Binary operators
///
/// All of C binary operators that can be applied to two expressions.
///
/// (C11 6.5.5 -- 6.5.16)
#[derive(Debug, PartialEq, Clone)]
pub struct BinaryOperatorExpression {
    pub operator: BinaryOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

/// Conditional operator
///
/// (C11 6.5.15)
#[derive(Debug, PartialEq, Clone)]
pub struct ConditionalExpression {
    pub condition: Box<Expression>,
    pub then_expression: Box<Expression>,
    pub else_expression: Box<Expression>,
}

/// Variable argument list access
///
/// Result of expansion of `va_arg` macro.
///
/// (C11 7.16.1.1).
#[derive(Debug, PartialEq, Clone)]
pub struct VaArgExpression {
    pub va_list: Box<Expression>,
    pub type_name: TypeName,
}

/// Member offset expression
///
/// Result of expansion of `offsetof` macro.
///
/// (C11 7.19 §3).
#[derive(Debug, PartialEq, Clone)]
pub struct OffsetOfExpression {
    pub type_name: TypeName,
    pub designator: OffsetDesignator,
}

/// Offset designator in a `offsetof` macro expansion
///
/// (C11 7.19 §3).
#[derive(Debug, PartialEq, Clone)]
pub struct OffsetDesignator {
    pub base: Identifier,
    pub members: Vec<OffsetMember>,
}

/// Single element of an offset designator
///
/// (C11 7.19 §3).
#[derive(Debug, PartialEq, Clone)]
pub enum OffsetMember {
    Member(Identifier),
    IndirectMember(Identifier),
    Index(Expression),
}

// From 6.7 Declarations

/// Variable, function or type declaration
///
/// (C11 6.7)
#[derive(Debug, PartialEq, Clone)]
pub struct Declaration {
    pub specifiers: Vec<DeclarationSpecifier>,
    pub declarators: Vec<InitDeclarator>,
}

/// Common part of a declaration
///
/// These apply to all declarators in a declaration.
///
/// (C11 6.7)
#[derive(Debug, PartialEq, Clone)]
pub enum DeclarationSpecifier {
    StorageClass(StorageClassSpecifier),
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
    Function(FunctionSpecifier),
    Alignment(AlignmentSpecifier),
    /// Vendor-specific declaration extensions that can be mixed with standard specifiers
    Extension(Vec<Extension>),
}

/// Defines a single name in a declaration
///
/// (C11 6.7.6)
#[derive(Debug, PartialEq, Clone)]
pub struct InitDeclarator {
    pub declarator: Declarator,
    pub initializer: Option<Initializer>,
}

// From 6.7.1

/// Storage class
///
/// (C11 6.7.1)
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum StorageClassSpecifier {
    /// `typedef`
    Typedef,
    /// `extern`
    Extern,
    /// `static`
    Static,
    /// `_Thread_local`
    ThreadLocal,
    /// `auto`
    Auto,
    /// `register`
    Register,
}

// From 6.7.2

/// Type specifier
///
/// (C11 6.7.2)
#[derive(Debug, PartialEq, Clone)]
pub enum TypeSpecifier {
    /// `void`
    Void,
    /// `char`
    Char,
    /// `short`
    Short,
    /// `int`
    Int,
    /// `long`
    Long,
    /// `float`
    Float,
    /// `double`
    Double,
    /// `signed`
    ///
    /// `__signed`, `__signed__` (GNU extension)
    Signed,
    /// `unsigned`
    Unsigned,
    /// `_Bool`
    Bool,
    /// `_Complex`
    ///
    /// `__complex`, `__complex__` (GNU extension)
    Complex,
    /// `_Atomic(typename)`
    Atomic(TypeName),
    /// `struct identifier { … }`
    ///
    /// `union identifier { … }`
    Struct(StructType),
    /// `enum identifier { … }`
    Enum(EnumType),
    /// Name of a previously defined type
    TypedefName(Identifier),
    /// Specifies type of another type or expression
    ///
    /// [GNU extension](https://gcc.gnu.org/onlinedocs/gcc/Typeof.html)
    TypeOf(TypeOf),
    /// Floating point types with guaranteed width and representation
    ///
    /// `_Float16`, `_Float32`, `_Float64`, `_Float128`
    ///
    /// `_Float16x`, `_Float32x`, `_Float64x`, `_Float128x`
    ///
    /// `_Decimal16`, `_Decimal32`, `_Decimal64`, `_Decimal128`
    ///
    /// `_Decimal16x`, `_Decimal32x`, `_Decimal64x`, `_Decimal128x`
    ///
    /// [ISO/IEC TS 18661-3:2015](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1945.pdf)
    TS18661Float(TS18661FloatType),
}

/// Floating point type with guaranteed width and format
///
/// [ISO/IEC TS 18661-3:2015](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1945.pdf)
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct TS18661FloatType {
    pub format: TS18661FloatFormat,
    pub width: usize,
}

/// Floating point formats
///
/// [ISO/IEC TS 18661-3:2015](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1945.pdf)
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum TS18661FloatFormat {
    BinaryInterchange,
    BinaryExtended,
    DecimalInterchange,
    DecimalExtended,
}

// From 6.7.2.1

/// Structure or union type specifier
///
/// (C11 6.7.2.1)
#[derive(Debug, PartialEq, Clone)]
pub struct StructType {
    pub kind: StructKind,
    pub identifier: Option<Identifier>,
    /// List of structure of union members, when present.
    ///
    /// A [GNU extension](https://gcc.gnu.org/onlinedocs/gcc-8.1.0/gcc/Empty-Structures.html) allows the list to be empty.
    pub declarations: Option<Vec<StructDeclaration>>,
}

/// The only difference between a `struct` and a `union`
///
/// (C11 6.7.2.1)
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum StructKind {
    Struct,
    Union,
}

/// Single declaration in a struct or a union
///
/// (C11 6.7.2.1)
#[derive(Debug, PartialEq, Clone)]
pub enum StructDeclaration {
    Field(StructField),
    StaticAssert(StaticAssert),
}

/// Struct field declaration
#[derive(Debug, PartialEq, Clone)]
pub struct StructField {
    pub specifiers: Vec<SpecifierQualifier>,
    pub declarators: Vec<StructDeclarator>,
}

/// Type and qualifiers for a struct declaration
///
/// C11 also uses this type in a few other places.
///
/// (C11 6.7.2.1)
#[derive(Debug, PartialEq, Clone)]
pub enum SpecifierQualifier {
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
    Extension(Vec<Extension>),
}

/// Field declarator for a struct or a union
///
/// (C11 6.7.2.1)
#[derive(Debug, PartialEq, Clone)]
pub struct StructDeclarator {
    pub declarator: Option<Declarator>,
    pub bit_width: Option<Box<Expression>>,
}

// From 6.7.2.2

/// Enumeration type specifier
///
/// (C11 6.7.2.2)
#[derive(Debug, PartialEq, Clone)]
pub struct EnumType {
    pub identifier: Option<Identifier>,
    pub enumerators: Vec<Enumerator>,
}

/// Single constant inside a `enum` definition
///
/// (C11 6.7.2.2)
#[derive(Debug, PartialEq, Clone)]
pub struct Enumerator {
    pub identifier: Identifier,
    pub expression: Option<Box<Expression>>,
    pub extensions: Vec<Extension>,
}

// From 6.7.3

/// Type qualifier
///
/// (C11 6.7.3)
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum TypeQualifier {
    /// `const`
    ///
    /// `__const` (GNU extension)
    Const,
    /// `restrict`
    ///
    /// `__restrict`, `__restrict__` (GNU extension)
    Restrict,
    /// `volatile`
    ///
    /// `__volatile`, `__volatile__` (GNU extension)
    Volatile,
    /// '_Nonnull' (Clang extension)
    ///
    /// [Clang extension](https://clang.llvm.org/docs/AttributeReference.html)
    Nonnull,
    /// '_Null_unspecified' (Clang extension)
    ///
    /// [Clang extension](https://clang.llvm.org/docs/AttributeReference.html)
    NullUnspecified,
    /// '_Nullable' (Clang extension)
    ///
    /// [Clang extension](https://clang.llvm.org/docs/AttributeReference.html)
    Nullable,
    /// `_Atomic`
    Atomic,
}

// From 6.7.4

/// Function specifier
///
/// (C11 6.7.4)
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum FunctionSpecifier {
    /// `inline`
    ///
    /// `__inline`, `__inline__` (GNU extension)
    Inline,
    /// `_Noreturn`
    Noreturn,
}

// From 6.7.5

/// Alignment specifier
///
/// (C11 6.7.5)
#[derive(Debug, PartialEq, Clone)]
pub enum AlignmentSpecifier {
    /// `_Alignas(typename)`
    Type(TypeName),
    /// `_Alignas(expression)`
    Constant(Box<Expression>),
}

// From 6.7.6 Declarators

/// Single item in a declaration
///
/// Represents both normal and abstract declarators.
///
/// (C11 6.7.6, 6.7.7)
#[derive(Debug, PartialEq, Clone)]
pub struct Declarator {
    /// What is being declared
    pub kind: DeclaratorKind,
    /// Contains pointer, array and function declarator elements
    pub derived: Vec<DerivedDeclarator>,
    /// Vendor-specific extensions
    pub extensions: Vec<Extension>,
}

/// Name of a declarator
///
/// (C11 6.7.6, 6.7.7)
#[derive(Debug, PartialEq, Clone)]
pub enum DeclaratorKind {
    /// Unnamed declarator
    ///
    /// E.g. part of a function prototype without parameter names.
    Abstract,
    /// Named declarator
    ///
    /// E.g. a variable or a named function parameter.
    Identifier(Identifier),
    /// Nested declarator
    ///
    /// Any group of parenthesis inside a declarator. E.g. pointer to
    /// a function.
    Declarator(Box<Declarator>),
}

/// Modifies declarator type
///
/// (C11 6.7.6)
#[derive(Debug, PartialEq, Clone)]
pub enum DerivedDeclarator {
    /// `* qualifiers …`
    Pointer(Vec<PointerQualifier>),
    /// `… []`
    Array(ArrayDeclarator),
    /// `… ( parameters )`
    Function(FunctionDeclarator),
    /// `… ( identifiers )`
    KRFunction(Vec<Identifier>),
    /// `^ qualifiers …`
    ///
    /// [Clang extension](https://clang.llvm.org/docs/BlockLanguageSpec.html)
    Block(Vec<PointerQualifier>),
}

/// Array part of a declarator
#[derive(Debug, PartialEq, Clone)]
pub struct ArrayDeclarator {
    pub qualifiers: Vec<TypeQualifier>,
    pub size: ArraySize,
}

/// Function parameter part of a declarator
#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclarator {
    pub parameters: Vec<ParameterDeclaration>,
    pub ellipsis: Ellipsis,
}

/// List of qualifiers that can follow a `*` in a declaration
///
/// (C11 6.7.6.1)
#[derive(Debug, PartialEq, Clone)]
pub enum PointerQualifier {
    TypeQualifier(TypeQualifier),
    Extension(Vec<Extension>),
}

/// Size of an array in a declaration
///
/// (C11 6.7.6.2)
#[derive(Debug, PartialEq, Clone)]
pub enum ArraySize {
    /// `[]`
    Unknown,
    /// `[*]`
    VariableUnknown,
    /// `[10]`
    VariableExpression(Box<Expression>),
    /// `[static 10]`
    StaticExpression(Box<Expression>),
}

/// Complete parameter declaration in a function prototype or declaration
///
/// This is so called "new-style" or "C89" parameter declaration that
/// follows in parenthesis after a function name. "Old-style" or "K&R"
/// function parameter declarations are collected in the
/// `FunctionDefinition::declarations` field.
///
/// (C11 6.7.6.3)
#[derive(Debug, PartialEq, Clone)]
pub struct ParameterDeclaration {
    pub specifiers: Vec<DeclarationSpecifier>,
    pub declarator: Option<Declarator>,
    pub extensions: Vec<Extension>,
}

/// Whether function signature ends with a `...`
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Ellipsis {
    Some,
    None,
}

// From 6.7.7 Type names

/// References to types outside of declarations
///
/// Type names contain only abstract declarators.
///
/// (C11 6.7.7)
#[derive(Debug, PartialEq, Clone)]
pub struct TypeName {
    pub specifiers: Vec<SpecifierQualifier>,
    pub declarator: Option<Declarator>,
}

// From 6.7.9 Initialization

/// Value that is assigned immediately in a declaration
///
/// (C11 6.7.9)
#[derive(Debug, PartialEq, Clone)]
pub enum Initializer {
    Expression(Box<Expression>),
    List(Vec<InitializerListItem>),
}

/// Initializes one field or array element in a initializer list
///
/// (C11 6.7.9)
#[derive(Debug, PartialEq, Clone)]
pub struct InitializerListItem {
    pub designation: Vec<Designator>,
    pub initializer: Box<Initializer>,
}

/// Single element of an designation in an initializer
#[derive(Debug, PartialEq, Clone)]
pub enum Designator {
    /// Array element
    ///
    /// `{ [expression] = … }`
    ///
    /// `{ [expression] … }` (obsolete GNU extension)
    Index(Expression),

    /// Struct or union member
    ///
    /// `{ .identifier = … }`
    ///
    /// `{ identifier: … }` (obsolete GNU extension)
    Member(Identifier),

    /// Range of array elements
    ///
    /// `{ [from ... to] … }`
    /// ([GNU extension](https://gcc.gnu.org/onlinedocs/gcc/Designated-Inits.html#Designated-Inits))
    Range(RangeDesignator),
}

/// Range array designator in an initializer
///
/// `[from ... to]`
///
/// ([GNU extension](https://gcc.gnu.org/onlinedocs/gcc/Designated-Inits.html#Designated-Inits))
#[derive(Debug, PartialEq, Clone)]
pub struct RangeDesignator {
    pub from: Expression,
    pub to: Expression,
}

// From 6.7.10 Static assertions

/// Static assertion
///
/// (C11 6.7.10)
#[derive(Debug, PartialEq, Clone)]
pub struct StaticAssert {
    pub expression: Box<Expression>,
    pub message: StringLiteral,
}

// From 6.8 Statement

/// Element of a function body
///
/// (C11 6.8)
#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Labeled(LabeledStatement),
    Compound(Vec<BlockItem>),
    Expression(Option<Box<Expression>>),
    If(IfStatement),
    Switch(SwitchStatement),
    While(WhileStatement),
    DoWhile(DoWhileStatement),
    For(ForStatement),
    Goto(Identifier),
    Continue,
    Break,
    Return(Option<Box<Expression>>),
    /// Vendor specific inline assembly extensions
    Asm(AsmStatement),
}

/// Labeled statement
///
/// (C11 6.8.1)
#[derive(Debug, PartialEq, Clone)]
pub struct LabeledStatement {
    pub label: Label,
    pub statement: Box<Statement>,
}

/// If statement
///
/// (C11 6.8.4)
#[derive(Debug, PartialEq, Clone)]
pub struct IfStatement {
    pub condition: Box<Expression>,
    pub then_statement: Box<Statement>,
    pub else_statement: Option<Box<Statement>>,
}

/// Switch statement
///
/// (C11 6.8.4)
#[derive(Debug, PartialEq, Clone)]
pub struct SwitchStatement {
    pub expression: Box<Expression>,
    pub statement: Box<Statement>,
}

/// While statement
///
/// (C11 6.8.5)
#[derive(Debug, PartialEq, Clone)]
pub struct WhileStatement {
    pub expression: Box<Expression>,
    pub statement: Box<Statement>,
}

/// Do statement
///
/// (C11 6.8.5)
#[derive(Debug, PartialEq, Clone)]
pub struct DoWhileStatement {
    pub statement: Box<Statement>,
    pub expression: Box<Expression>,
}

/// For statement
///
/// (C11 6.8.5)
#[derive(Debug, PartialEq, Clone)]
pub struct ForStatement {
    pub initializer: ForInitializer,
    pub condition: Option<Box<Expression>>,
    pub step: Option<Box<Expression>>,
    pub statement: Box<Statement>,
}

/// Statement labels for `goto` and `switch`
#[derive(Debug, PartialEq, Clone)]
pub enum Label {
    /// Goto label
    ///
    /// `ident: …`
    Identifier(Identifier),
    /// Case in a `switch` statement
    ///
    /// `case 'a': …`
    Case(Box<Expression>),
    /// Case with a range in a `switch` statement
    ///
    /// `case 'a' ... 'z': …`
    ///
    /// [GNU extension](https://gcc.gnu.org/onlinedocs/gcc/Case-Ranges.html)
    CaseRange(CaseRange),
    /// Default case in a `switch` statement
    ///
    /// `default: …`
    Default,
}

/// Case range expression
///
/// `from ... to`
///
/// [GNU extension](https://gcc.gnu.org/onlinedocs/gcc/Case-Ranges.html)
#[derive(Debug, PartialEq, Clone)]
pub struct CaseRange {
    pub low: Box<Expression>,
    pub high: Box<Expression>,
}

/// First element of a `for` statement
#[derive(Debug, PartialEq, Clone)]
pub enum ForInitializer {
    /// `for(; …)`
    Empty,
    /// `for(a = 1; …)`
    Expression(Box<Expression>),
    /// `for(int a = 1; …)`
    Declaration(Declaration),
    /// `for(_StaticAssert(…); …)`
    StaticAssert(StaticAssert),
}

// From 6.8.2

/// Element of a compound statement
#[derive(Debug, PartialEq, Clone)]
pub enum BlockItem {
    Declaration(Declaration),
    StaticAssert(StaticAssert),
    Statement(Statement),
}

// From 6.9 External definitions

/// Entire C source file after preprocessing
///
/// (C11 6.9)
#[derive(Debug, PartialEq, Clone)]
pub struct TranslationUnit(pub Vec<ExternalDeclaration>);

/// Top-level elements of a C program
///
/// (C11 6.9)
#[derive(Debug, PartialEq, Clone)]
pub enum ExternalDeclaration {
    Declaration(Declaration),
    StaticAssert(StaticAssert),
    FunctionDefinition(FunctionDefinition),
}

/// Function definition
///
/// (C11 6.9.1)
#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefinition {
    /// Return type of the function, possibly mixed with other specifiers
    pub specifiers: Vec<DeclarationSpecifier>,
    /// Contains function name and parameter list
    pub declarator: Declarator,
    /// K&R style parameter type definitions (C11 6.9.1 §6)
    pub declarations: Vec<Declaration>,
    /// Body of the function.
    pub statement: Statement,
}

// Syntax extensions

/// Extended vendor-specific syntax that does not fit elsewhere
#[derive(Debug, PartialEq, Clone)]
pub enum Extension {
    /// Attributes
    ///
    /// [GNU extension](https://gcc.gnu.org/onlinedocs/gcc/Attribute-Syntax.html)
    Attribute(Attribute),
    /// Assembler name for an object
    ///
    /// [GNU extension](https://gcc.gnu.org/onlinedocs/gcc/Asm-Labels.html)
    AsmLabel(StringLiteral),
    /// Platform availability
    ///
    /// [Clang extension](https://clang.llvm.org/docs/AttributeReference.html#availability)
    AvailabilityAttribute(AvailabilityAttribute),
}

/// Attributes
///
/// [GNU extension](https://gcc.gnu.org/onlinedocs/gcc/Attribute-Syntax.html)
#[derive(Debug, PartialEq, Clone)]
pub struct Attribute {
    pub name: String,
    pub arguments: Vec<Expression>,
}

/// Platform availability attribute
///
/// [Clang extension](https://clang.llvm.org/docs/AttributeReference.html#availability)
#[derive(Debug, PartialEq, Clone)]
pub struct AvailabilityAttribute {
    pub platform: Identifier,
    pub clauses: Vec<AvailabilityClause>,
}

/// Platfrom availability attribute clause
///
/// [Clang extension](https://clang.llvm.org/docs/AttributeReference.html#availability)
#[derive(Debug, PartialEq, Clone)]
pub enum AvailabilityClause {
    Introduced(AvailabilityVersion),
    Deprecated(AvailabilityVersion),
    Obsoleted(AvailabilityVersion),
    Unavailable,
    Message(StringLiteral),
    Replacement(StringLiteral),
}

/// Platfrom version inside availability attribute
///
/// [Clang extension](https://clang.llvm.org/docs/AttributeReference.html#availability)
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct AvailabilityVersion {
    pub major: String,
    pub minor: Option<String>,
    pub subminor: Option<String>,
}

/// Inline assembler
#[derive(Debug, PartialEq, Clone)]
pub enum AsmStatement {
    /// Basic asm statement with just source code
    ///
    /// [GNU extension](https://gcc.gnu.org/onlinedocs/gcc/Basic-Asm.html)
    GnuBasic(StringLiteral),

    /// Extended statement that has access to C variables
    ///
    /// [GNU extension](https://gcc.gnu.org/onlinedocs/gcc/Extended-Asm.html)
    GnuExtended(GnuExtendedAsmStatement),
}

/// Extended statement that has access to C variables
///
/// [GNU extension](https://gcc.gnu.org/onlinedocs/gcc/Extended-Asm.html)
#[derive(Debug, PartialEq, Clone)]
pub struct GnuExtendedAsmStatement {
    pub qualifier: Option<TypeQualifier>,
    pub template: StringLiteral,
    pub outputs: Vec<GnuAsmOperand>,
    pub inputs: Vec<GnuAsmOperand>,
    pub clobbers: Vec<StringLiteral>,
}

/// Single input or output operand specifier for GNU extended asm statement
///
/// [GNU extension](https://gcc.gnu.org/onlinedocs/gcc/Extended-Asm.html#Output-Operands)
#[derive(Debug, PartialEq, Clone)]
pub struct GnuAsmOperand {
    pub symbolic_name: Option<Identifier>,
    pub constraints: StringLiteral,
    pub variable_name: Expression,
}

/// Type of an expression or type
///
/// [GNU extension](https://gcc.gnu.org/onlinedocs/gcc/Typeof.html)
#[derive(Debug, PartialEq, Clone)]
pub enum TypeOf {
    Expression(Expression),
    Type(TypeName),
}
