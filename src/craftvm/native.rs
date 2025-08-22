use std::time::UNIX_EPOCH;

use crate::craftvm::value::{CrNativeArgs, CrValue};

// available in NTALLOCA as function 0
// Similar to how we present it in Ocaml - as a timestamp in seconds with some decimal places
pub fn clock(_v: CrNativeArgs) -> CrValue {
    unsafe {
        // may not work in some wasm environments ??
        let t = std::time::SystemTime::now().duration_since(UNIX_EPOCH).unwrap_unchecked();
        CrValue::CrNumber(t.as_secs_f64())
    }
}
