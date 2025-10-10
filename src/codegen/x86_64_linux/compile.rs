//! The actual compilation.

use super::{CodegenPacket, ResultPacket};
use std::sync::{
    mpmc::{Receiver, Sender},
};

pub fn compile_thread<'a>(
    new_block_chan: Receiver<CodegenPacket<'a>>,
    result_chan: Sender<ResultPacket<'a>>,
) -> impl Fn() {
    move || {
        for packet in new_block_chan.iter() {

        }
    }
}
