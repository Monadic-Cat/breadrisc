use log::info;
use std::env;
use std::fs::File;
use std::io::Read;

use breadrisc::eight::vm::Machine;

fn main() {
    env_logger::init();

    let mut args = env::args();
    args.next();
    let filename = args.next().unwrap();
    info!("Filename: {:#?}", filename);
    let mut file = File::open(filename).expect("Something went wrong reading the file");
    info!("File: {:#?}", file);
    let image = {
        let mut image = Vec::<u8>::new();
        file.read_to_end(&mut image).unwrap();
        image
    };
    let mut mach = Machine::from(image.as_slice());
    loop {
        mach.tick().unwrap();
    }
}
