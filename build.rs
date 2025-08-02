pub fn main() -> std::io::Result<()> {
    // for properly linking on MacOS!
    if cfg!(any(target_os = "macos", target_os = "ios")) {
        println!("cargo:rustc-link-arg=-Wl,-undefined,dynamic_lookup");
    }
    ocaml_build::Sigs::new("lib/craftvm.ml").generate()
}
