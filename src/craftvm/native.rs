use std::time::UNIX_EPOCH;

use rand::Rng;

use crate::craftvm::value::{CrNativeArgs, CrValue};

// TODO: should probably use Result for return types here but meh

// available in NTALLOCA as function 0
// Similar to how we present it in Ocaml - as a timestamp in seconds with some decimal places
pub fn clock(_v: CrNativeArgs) -> CrValue {
    unsafe {
        // may not work in some wasm environments ??
        let t = std::time::SystemTime::now().duration_since(UNIX_EPOCH).unwrap_unchecked();
        CrValue::CrNumber(t.as_secs_f64())
    }
}

// available in NTALLOCA as function 1
// Similar to how we present it in Ocaml - as a timestamp in seconds with some decimal places
pub fn rand(v: CrNativeArgs) -> CrValue {
    // NB: the arguments will be in reverse!
    match (v[1], v[0]) {
        (CrValue::CrNumber(min), CrValue::CrNumber(max)) => {
            let r: f64 = rand::thread_rng().gen_range(min..=max);
            CrValue::CrNumber(r)
        },
        // if we get wron args - we just return nil and don't fail. bad design but whatever
        _ => CrValue::CrNil
    }
}

// available in NTALLOCA as function 2
// Similar to how we present it in Ocaml - as a timestamp in seconds with some decimal places
pub fn sqrt(v: CrNativeArgs) -> CrValue {
    // NB: the arguments will be in reverse!
    match v[0] {
        CrValue::CrNumber(n) => {
            CrValue::CrNumber(n.sqrt())
        },
        // if we get wron args - we just return nil and don't fail. bad design but whatever
        _ => CrValue::CrNil
    }
}
