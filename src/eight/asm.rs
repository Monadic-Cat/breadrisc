//! Assembler library for BreadRISC8.

use super::base::{
    Add, Instruction, Mem, Miz, MyFrom, NRegisterAddress, Nand, RegisterAddress,
    RegisterAddressOverflow, IO,
};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    error::ErrorKind::TooLarge,
    multi::{many0, many1},
    sequence::tuple,
    Err::Failure,
    IResult,
};
use std::convert::TryFrom;
use std::iter::once;
use thiserror::Error;
use typenum::{U2, U3};
use std::collections::HashMap;

// From mice:
fn whitespace(input: &str) -> IResult<&str, &str> {
    alt((tag(" "), tag("\t")))(input)
}

/// Integer bases
enum Base {
    Hexadecimal,
    Octal,
    Binary,
    Decimal,
}
impl Base {
    fn radix(&self) -> u8 {
        match self {
            Base::Hexadecimal => 16,
            Base::Binary => 2,
            Base::Octal => 8,
            Base::Decimal => 10,
        }
    }
}

/// Parser for a hexadecimal (Base 16) integer
fn hex(input: &str) -> IResult<&str, Base> {
    let (input, _) = tag("0x")(input)?;
    Ok((input, Base::Hexadecimal))
}
/// Parser for a binary (Base 2) integer
fn bin(input: &str) -> IResult<&str, Base> {
    let (input, _) = tag("b")(input)?;
    Ok((input, Base::Binary))
}
/// Parser for an octal (Base 8) integer
fn oct(input: &str) -> IResult<&str, Base> {
    let (input, _) = tag("0")(input)?;
    Ok((input, Base::Octal))
}
/// Parser for an integer base prefix.
fn integer_base(input: &str) -> IResult<&str, u8> {
    let (input, base) = match alt((hex, bin, oct))(input) {
        Ok((input, base)) => (input, base.radix()),
        // If no base prefix exists, try decimal.
        Err(_) => (input, Base::Decimal.radix()),
    };
    Ok((input, base))
}

// fn alt_gates_2<F, G, J, K, L, I, T, V, W>(
//     first: (F, G),
//     second: (J, K),
// ) -> impl Fn(I) -> IResult<I, W>
// where
//     F: Fn(I) -> IResult<I, T>,
//     G: Fn(I) -> IResult<I, W>,
//     J: Fn(I) -> IResult<I, V>,
//     K: Fn(I) -> IResult<I, W>,
//     I: Copy,
// {
//     let (first_gate, first_path) = first;
//     let (second_gate, second_path) = second;
//     move |input| {
//         let a = first_gate(input);
//         if let Ok((i, _)) = a {
//             first_path(i)
//         } else {
//             let a = second_gate(input);
//             if let Ok((i, _)) = a {
//                 second_path(i)
//             } else {
//                 Err(nom::Err::Error((input, nom::error::ErrorKind::Alt)))
//             }
//         }
//     }
// }

// macro_rules! alt_gates {

// }

fn is_digit(base: u8) -> impl Fn(char) -> bool {
    move |c| c.is_digit(base as u32)
}

fn integer(input: &str) -> IResult<&str, u8> {
    let (input, base) = integer_base(input)?;
    let (input, int) = take_while1(is_digit(base))(input)?;
    let i = match u8::from_str_radix(int, base as u32) {
        // The only error possible here is
        // integer overflow.
        // This should emit a nom Failure
        Err(_) => return Err(Failure((input, TooLarge))),
        Ok(x) => x,
    };
    Ok((input, i))
}

fn addr_res_to_nom<I>(
    input: I,
    res: Result<RegisterAddress, RegisterAddressOverflow>,
) -> Result<RegisterAddress, nom::Err<(I, nom::error::ErrorKind)>> {
    match res {
        Ok(x) => Ok(x),
        Err(_) => Err(Failure((input, TooLarge))),
    }
}
fn nom_addr<I>(input: I, idx: u8) -> Result<RegisterAddress, nom::Err<(I, nom::error::ErrorKind)>> {
    addr_res_to_nom(input, RegisterAddress::try_from(idx))
}

macro_rules! normal_instruction {
    ($($name: ident, $type_name: ident, $tag: expr);+) => {
        $(fn $name (input: &str) -> IResult<&str, Instruction>{
            let (input, (_, _, destination, _, _, source)) = tuple((
                tag($tag),
                many1(whitespace),
                integer, tag(","),
                many0(whitespace),
                integer))(input)?;
            Ok((
                input,
                Instruction::$type_name($type_name {
                    destination: nom_addr(input, destination)?,
                    source: nom_addr(input, source)?,
                })
            ))
        })*
    }
}
normal_instruction!(add, Add, "add";
                    nand, Nand, "nand";
                    miz, Miz, "miz");

fn io_write(input: &str) -> IResult<&str, IO> {
    let (input, _) = tag("w")(input)?;
    Ok((input, IO::Write))
}
fn io_read(input: &str) -> IResult<&str, IO> {
    let (input, _) = tag("r")(input)?;
    Ok((input, IO::Read))
}
fn io_direction(input: &str) -> IResult<&str, IO> {
    alt((io_write, io_read))(input)
}

fn mem(input: &str) -> IResult<&str, Instruction> {
    let (input, (_, _, destination, _, _, source, _, _, direction)) = tuple((
        tag("mem"),
        many1(whitespace),
        integer,
        tag(","),
        many0(whitespace),
        integer,
        tag(","),
        many0(whitespace),
        io_direction,
    ))(input)?;
    Ok((
        input,
        Instruction::Mem(Mem {
            destination: nom_addr(input, destination)?,
            source: match NRegisterAddress::<U2>::try_from(source) {
                Ok(x) => x,
                Err(_) => return Err(Failure((input, TooLarge))),
            },
            direction,
        }),
    ))
}

fn instruction(input: &str) -> IResult<&str, Instruction> {
    alt((add, nand, miz, mem))(input)
}

/// Instruction lines can be separated by lines full of
/// any amount of whitespace, but no other kinds of character.
/// Instructions MUST be separated by at least one newline.
fn instruction_sep(input: &str) -> IResult<&str, Vec<(&str, Vec<&str>)>> {
    many1(tuple((tag("\n"), many0(whitespace))))(input)
}

/// Directives of the form `#[kind(args...)]`
///
/// `#[pad(100, 0)]` will insert 100 null bytes.
///
/// `#[pad_until(0x100, 0)]` will insert null bytes
/// until it reaches address `0x100` in memory.
///
/// `#[label("heck")]` will define a label `heck`
/// for a deferred instruction to use.
/// The semantics of labels are not fully defined yet.
/// Using a label across hard page boundaries is not permitted.
///
/// `#[place_label("heck")]` will place the byte address of label `heck`
/// into the binary at the current position.
///
/// `#[ord(n)]` will cause all labels to be offset `n` bytes.
///
/// `#[hard_ord(n)]` will cause all memory addresses to be offset `n` bytes.
/// There may only be one `ord` or `hard_ord` directive in a program.
///
/// Without an `ord`-type directive, labels assume 0 offset.
/// Additionally, it is an error for any offset + label address combination
/// to be greater than 255.
enum Directive {
    /// Variable size in the final binary.
    Pad { amount: usize, content: char },
    /// Variable size in the final binary.
    PadUntil { end: usize, content: char },
    /// Zero size in the final binary.
    DefineLabel { name: String },
    /// One byte in the final binary.
    UseLabel { name: String },
    /// Zero size in the final binary.
    Ord { offset: u8 },
    /// Zero size in the final binary.
    HardOrd { offset: u8 },
}

/// Parser for a program directive
fn directive(input: &str) -> IResult<&str, Directive> {
    let (input, _) = tag("#")(input)?;
    unimplemented!()
}
/// Instruction that requires information
/// about the rest of the program to be assembled correctly.
struct DeferredInstruction {}

#[derive(Debug, Error)]
enum ResolverError {
    #[error("failed to resolve label")]
    MissingLabel,
    #[error("attempted to use label across hard page boundary")]
    InvalidLabel,
}

struct Label {
    name: String,
}
struct Address {
    index: u8,
    /// Kept internally to allow checking for invalid label usage.
    /// Note that a page is half the available address space;
    /// the hard page boundaries are the -1 : 0  and 255 : 256
    /// Behind the 0th and after the 2nd pages, relative to the current one.
    page_offset: u8,
}
/// The two kinds of offset are mutually exclusive
/// because I think their interaction would be confusing.
enum AddressOffset {
    Label(u8),
    Global(u8),
}

/// Global information about the program.
/// Labels, label offset, and global offset.
struct ProgInfo {
    labels: HashMap<Label, Address>,
    offset: AddressOffset,
}

impl DeferredInstruction {
    fn finalize(&mut self, prog: ProgInfo) -> Result<Instruction, ResolverError> {
        unimplemented!()
    }
}

/// Instruction that may or may not require
/// information about the rest of the program
/// to be assembled correctly.
enum EventualInstruction {
    Immediate(Instruction),
    Deferred(DeferredInstruction),
}

impl EventualInstruction {
    fn finalize(&mut self, prog: ProgInfo) -> Result<Instruction, ResolverError> {
        match self {
            EventualInstruction::Immediate(x) => Ok(*x),
            EventualInstruction::Deferred(x) => x.finalize(prog),
        }
    }
}
impl From<Instruction> for EventualInstruction {
    fn from(i: Instruction) -> Self {
        Self::Immediate(i)
    }
}

/// Parser for a comment
fn comment(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag(";")(input)?;
    take_while(|x| x != '\n')(input)
}

/// Sum of literal instructions and special elements
/// of a BreadRISC8 assembly program.
enum Node {
    Instruction(EventualInstruction),
    Directive(Directive),
    Comment(String),
}

/// Parser for an arbitrary AST node.
fn node(input: &str) -> IResult<&str, Node> {
    alt((
        |x| {
            let (i, v) = directive(x)?;
            Ok((i, Node::Directive(v)))
        },
        |x| {
            let (i, v) = instruction(x)?;
            Ok((i, Node::Instruction(v.into())))
        },
        |x| {
            let (i, v) = comment(x)?;
            Ok((i, Node::Comment(String::from(v))))
        },
    ))(input)
}

type Program = Vec<Instruction>;
/// Parser for a whole BreadRISC8 assembly program.
fn program(input: &str) -> IResult<&str, Program> {
    let (input, (first, rest)) =
        tuple((instruction, many0(tuple((instruction_sep, instruction)))))(input)?;

    let prog_it = once(first).chain(rest.into_iter().map(|x| x.1));
    Ok((input, prog_it.collect()))
}

/// Assemble a full BreadRISC8 program.
pub fn assemble(input: &str) -> Result<Vec<u8>, nom::Err<(&str, nom::error::ErrorKind)>> {
    let (_, prog) = program(input)?;

    Ok(prog.into_iter().map(|inst| u8::from(inst)).collect())
}

#[cfg(test)]
mod test {
    use super::assemble;
    #[test]
    fn valid_assembly() {
        assert_eq!(assemble("add 3, 5").unwrap(), vec![0b00_011_101u8]);
        assert_eq!(assemble("add 0x3, 0x5").unwrap(), vec![0b00_011_101u8]);
        assert_eq!(assemble("add b11, b101").unwrap(), vec![0b00_011_101u8]);
        assert_eq!(assemble("nand 3, 5").unwrap(), vec![0b01_011_101u8]);
        assert_eq!(assemble("mem 3, 3, w").unwrap(), vec![0b10_011_1_11u8]);
        assert_eq!(assemble("mem 3, 3, r").unwrap(), vec![0b10_011_0_11u8]);
        assert_eq!(assemble("miz 3, 5").unwrap(), vec![0b11_011_101u8]);
    }
    #[test]
    #[should_panic]
    fn invalid_add() {
        assemble("add 3, 8").unwrap();
    }
    #[test]
    #[should_panic]
    fn invalid_nand() {
        assemble("nand 3, 8").unwrap();
    }
    #[test]
    #[should_panic]
    fn invalid_mem() {
        assemble("mem 3, 4, w").unwrap();
    }
    #[test]
    #[should_panic]
    fn invalid_miz() {
        assemble("miz 3, 8").unwrap();
    }
}
