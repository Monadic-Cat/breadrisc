use log::info;
use std::convert::TryFrom;
use std::error::Error;
use std::fmt;
use std::fmt::{Display, Formatter};

use std::marker::PhantomData;
use typenum::{Unsigned,U2, U3, IsGreater};

#[derive(Debug)]
pub(crate) struct RegisterAddressOverflow {}
impl Display for RegisterAddressOverflow {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Too large")
    }
}
impl Error for RegisterAddressOverflow {}
impl From<RegisterAddressOverflow> for &str {
    fn from(_: RegisterAddressOverflow) -> &'static str {
        "Too large"
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct NRegisterAddress<N: Unsigned> {
    index: u8,
    _bound: PhantomData<N>,
}
impl<N: Unsigned> TryFrom<u8> for NRegisterAddress<N> {
    type Error = RegisterAddressOverflow;
    fn try_from(p: u8) -> Result<Self, Self::Error> {
        if p <= ((1 << N::to_u8()) - 1) {
            Ok(NRegisterAddress {
                index: p,
                _bound: PhantomData,
            })
        } else {
            Err(RegisterAddressOverflow {})
        }
    }
}
impl<N: Unsigned> From<NRegisterAddress<N>> for u8 {
    fn from(r: NRegisterAddress<N>) -> u8 {
        r.index
    }
}
impl<N: Unsigned> From<NRegisterAddress<N>> for usize {
    fn from(r: NRegisterAddress<N>) -> usize {
        r.index as usize
    }
}

/// DO NOT LET THIS GET OUTSIDE THE CRATE.
/// SCREAMING NO OH GOD THAT'D BE TERRIBLE
/// AND CONFUSING.
pub(crate) trait MyFrom<T> {
    fn my_from(_: T) -> Self;
}
impl<N: Unsigned, M> MyFrom<NRegisterAddress<N>> for NRegisterAddress<M>
where
    M: Unsigned + IsGreater<N>,
{
    fn my_from(r: NRegisterAddress<N>) -> Self {
        Self { index: r.index, _bound: PhantomData }
    }
}
pub(crate) type RegisterAddress = NRegisterAddress<U3>;

// #[derive(Debug, Clone, Copy)]
// pub(crate) struct RegisterAddress(u8);
// impl RegisterAddress {
//     pub(crate) fn new(p: u8) -> Result<RegisterAddress, RegisterAddressOverflow> {
//         if p <= 7 {
//             Ok(RegisterAddress(p))
//         } else {
//             Err(RegisterAddressOverflow {})
//         }
//     }
// }
// impl From<RegisterAddress> for usize {
//     fn from(a: RegisterAddress) -> Self {
//         let a = a.0;
//         a as Self
//     }
// }
// impl From<NRegisterAddress<U3>> for RegisterAddress {
//     fn from(r: NRegisterAddress<U3>) -> Self {
//         Self(r.index)
//     }
// }

#[derive(Debug, Clone, Copy)]
pub(crate) enum IO {
    Read,
    Write,
}
#[derive(Debug, Clone, Copy)]
pub(crate) struct Add {
    pub(crate) destination: RegisterAddress,
    pub(crate) source: RegisterAddress,
}
#[derive(Debug, Clone, Copy)]
pub(crate) struct Nand {
    pub(crate) destination: RegisterAddress,
    pub(crate) source: RegisterAddress,
}
#[derive(Debug, Clone, Copy)]
pub(crate) struct Mem {
    pub(crate) destination: RegisterAddress,
    pub(crate) source: NRegisterAddress<U2>,
    pub(crate) direction: IO,
}
#[derive(Debug, Clone, Copy)]
pub(crate) struct Miz {
    pub(crate) destination: RegisterAddress,
    pub(crate) source: RegisterAddress,
}
#[derive(Debug, Clone, Copy)]
pub(crate) enum Instruction {
    Add(Add),
    Nand(Nand),
    Mem(Mem),
    Miz(Miz),
}

/// Need this for the VM.
impl TryFrom<u8> for Instruction {
    type Error = &'static str;
    fn try_from(code: u8) -> Result<Self, Self::Error> {
        info!("Instruction Code: b{:08b}", code);
        let op = (code & (0b11 << 6)) >> 6;
        let op = if op == 0 {
            Instruction::Add(Add {
                destination: RegisterAddress::try_from((code & (0b111 << 3)) >> 3)?,
                source: RegisterAddress::try_from(code & 0b111)?,
            })
        } else if op == 1 {
            Instruction::Nand(Nand {
                destination: RegisterAddress::try_from((code & (0b111 << 3)) >> 3)?,
                source: RegisterAddress::try_from(code & 0b111)?,
            })
        } else if op == 2 {
            Instruction::Mem(Mem {
                destination: RegisterAddress::try_from((code & (0b111 << 3)) >> 3)?,
                direction: if (code & (0b1 << 2)) >> 2 == 1 {
                    IO::Write
                } else {
                    IO::Read
                },
                source: NRegisterAddress::<U2>::try_from(code & 0b11)?,
            })
        } else if op == 3 {
            Instruction::Miz(Miz {
                destination: RegisterAddress::try_from((code & (0b111 << 3)) >> 3)?,
                source: RegisterAddress::try_from(code & 0b111)?,
            })
        } else {
            panic!("Something went terribly wrong.")
        };
        Ok(op)
    }
}

/// Need this for the assembler.
impl From<Instruction> for u8 {
    fn from(inst: Instruction) -> u8 {
        match inst {
            Instruction::Add(Add {
                destination,
                source,
            }) => 0 << 6 | destination.index << 3 | source.index,
            Instruction::Nand(Nand {
                destination,
                source,
            }) => 1 << 6 | destination.index << 3 | source.index,
            Instruction::Mem(Mem {
                destination,
                source,
                direction,
            }) => {
                2 << 6
                    | destination.index << 3
                    | match direction {
                        IO::Read => 0,
                        IO::Write => 1,
                    } << 2
                    | source.index
            }
            Instruction::Miz(Miz {
                destination,
                source,
            }) => 3 << 6 | destination.index << 3 | source.index,
        }
    }
}
