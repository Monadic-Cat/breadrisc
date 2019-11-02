use super::base::{Add, Instruction, Mem, Miz, Nand, RegisterAddress, IO, MyFrom};
use log::info;
use std::convert::{TryFrom, TryInto};
use std::io::{Read, Write};
use std::ops::{Index, IndexMut};

#[derive(Default, Debug)]
struct Registers([u8; 8]);
// Special registers
const CONDITION_REGISTER: usize = 4;
const SHIFT_REGISTER: usize = 5;
const PAGE_REGISTER: usize = 6;
const INSTRUCTION_POINTER: usize = 7;

impl Registers {
    /// Do special actions for CPU cycle
    fn tick(&mut self) {
        self.0[SHIFT_REGISTER] >>= 1;
        self.0[INSTRUCTION_POINTER] = self.0[INSTRUCTION_POINTER]
            .checked_add(1)
            .expect("Finished.");
    }
}
impl Index<RegisterAddress> for Registers {
    type Output = u8;
    fn index(&self, idx: RegisterAddress) -> &u8 {
        &self.0[usize::from(idx)]
    }
}
impl IndexMut<RegisterAddress> for Registers {
    fn index_mut(&mut self, idx: RegisterAddress) -> &mut u8 {
        &mut self.0[usize::from(idx)]
    }
}

const MEM_SIZE: usize = 0xFFFF;
#[allow(dead_code)]
const IO_START: usize = 0x100;
const IO_COMMAND_BYTE: usize = 0x100;
const IO_DATA_BYTE: usize = 0x101;
#[allow(dead_code)]
const IO_END: usize = 0x13F;
const IO_COMMAND_WRITE: u8 = 1;
const IO_COMMAND_READ: u8 = 2;

/// BreadRISC virtual machine.
#[derive(Debug)]
struct Memory {
    ram: Vec<u8>,
    // This field must be updated with the page register.
    // Alternatively, we could use a Cell on the registers.
    page: u8,
    io: bool,
}
impl Memory {
    fn new() -> Memory {
        Memory {
            ram: Vec::with_capacity(MEM_SIZE),
            page: 0,
            io: false,
        }
    }
    // Load system image.
    fn load_image(&mut self, image: &[u8]) {
        println!("Image size: {}", self.ram.len());
        self.ram.clear();
        for x in image {
            self.ram.push(*x);
        }
        println!("Image size: {}", self.ram.len());
    }
    fn tick(&mut self) {
        if self.io {
            if self.ram[IO_COMMAND_BYTE] == IO_COMMAND_WRITE {
                let stdout = std::io::stdout();
                let mut handle = stdout.lock();
                info!("Writing...");
                write!(handle, "{}", self.ram[IO_DATA_BYTE]).unwrap();
            } else if self.ram[IO_COMMAND_BYTE] == IO_COMMAND_READ {
                let mut stdin = std::io::stdin();
                let cell = std::slice::from_mut(&mut self.ram[IO_DATA_BYTE]);
                stdin.read(cell).unwrap();
                // Maybe do some error handling?
                // Warn if stdin is closed?
            }
            self.io = false;
            // Technically unnecessary, but...
            self.ram[IO_COMMAND_BYTE] = 0;
        }
    }
    fn page_offset(&self, idx: u8) -> usize {
        ((idx as u16) + ((self.page as u16) << 7))
            .try_into()
            .unwrap()
    }
}
impl Index<u8> for Memory {
    type Output = u8;
    fn index(&self, idx: u8) -> &u8 {
        &self.ram[self.page_offset(idx)]
    }
}
impl IndexMut<u8> for Memory {
    fn index_mut(&mut self, idx: u8) -> &mut u8 {
        let offset = self.page_offset(idx);
        if offset == IO_COMMAND_BYTE {
            info!("Setting up IO...");
            self.io = true;
        }
        &mut self.ram[offset]
    }
}

impl From<&[u8]> for Memory {
    fn from(data: &[u8]) -> Memory {
        let mut memory = Memory::new();
        memory.load_image(data);
        memory
    }
}

#[derive(Debug)]
pub struct Machine {
    regs: Registers,
    memory: Memory,
}
impl Machine {
    pub fn new() -> Machine {
        Machine {
            regs: Registers::default(),
            memory: Memory::new(),
        }
    }
    pub fn tick(&mut self) -> Result<(), &'static str> {
        // Fetch, decode, execute.
        let instruction_address = (self.regs.0)[7];
        info!(
            "Instruction Offset: 0x{:04X}",
            self.memory.page_offset(instruction_address)
        );
        let inst = Instruction::try_from(self.memory[instruction_address])?;
        info!("Running instruction: {:?}", inst);
        info!("Current registers state: {:?}", self.regs);
        self.exec(inst)?;
        info!("Current registers state: {:?}", self.regs);
        // Keep memory page in sync with page register.
        self.memory.page = self.regs.0[PAGE_REGISTER];
        // Lastly, update registers.
        self.regs.tick();
        self.memory.tick();
        info!("{}", "-------------------------------".repeat(3));
        Ok(())
    }
    fn exec(&mut self, inst: Instruction) -> Result<(), &'static str> {
        match inst {
            Instruction::Add(Add {
                destination,
                source,
            }) => {
                self.regs[destination] = self.regs[destination].wrapping_add(self.regs[source]);
                Ok(())
            }
            Instruction::Nand(Nand {
                destination,
                source,
            }) => {
                let operand = self.regs[source];
                let target = self.regs.index_mut(destination);
                *target &= operand;
                *target = !*target;
                Ok(())
            }
            Instruction::Mem(Mem {
                destination,
                source,
                direction,
            }) => match direction {
                IO::Read => {
                    info!(
                        "Reading from cell {:?} to register {:?}",
                        self.memory.page_offset(self.regs[RegisterAddress::my_from(source)]),
                        destination
                    );
                    self.regs[destination] = self.memory[self.regs[RegisterAddress::my_from(source)]];
                    Ok(())
                }
                IO::Write => {
                    info!(
                        "Writing {} to cell {}",
                        self.regs[RegisterAddress::my_from(source)],
                        self.memory.page_offset(self.regs[destination])
                    );
                    self.memory[self.regs[destination]] = self.regs[RegisterAddress::my_from(source)];
                    Ok(())
                }
            },
            Instruction::Miz(Miz {
                destination,
                source,
            }) => {
                if self.regs.0[CONDITION_REGISTER] == 0 {
                    self.regs[destination] = self.regs[source];
                }
                Ok(())
            }
        }
    }
}
impl From<&[u8]> for Machine {
    fn from(data: &[u8]) -> Machine {
        Machine {
            regs: Registers::default(),
            memory: Memory::from(data),
        }
    }
}
