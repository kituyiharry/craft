pub type CraftValue = f64;

pub struct ConstPool { 
    vals: Vec<CraftValue>
}

impl ConstPool {
    pub fn new() -> Self {
        Self { 
            vals: vec![]
        }
    }

    pub fn insert(&mut self, val: CraftValue) -> usize {
        let idx = self.vals.len();
        self.vals.push(val);
        idx
    }

    pub fn get(&self, idx: usize) -> CraftValue {
        self.vals[idx]
    }
}

impl Default for ConstPool {
    fn default() -> Self {
        ConstPool::new() 
    }
}
