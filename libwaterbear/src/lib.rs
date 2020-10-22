
extern crate waterbear;

use std::ffi::CStr;
use std::os::raw::c_char;
use std::slice;

#[no_mangle]
pub extern "C" fn waterbear_invoke_command_line(len: usize, ptr: *const *const c_char) {
    let args: Vec<String> = unsafe { slice::from_raw_parts(ptr, len) }.into_iter()
        .into_iter()
        .map(|c_char: &*const c_char| unsafe { CStr::from_ptr(*c_char) }.to_str().unwrap().to_owned() )
        .collect();
    waterbear::run_command(&args)
}

