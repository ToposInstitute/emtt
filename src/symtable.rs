use std::collections::HashMap;
use ustr::Ustr;

pub struct Checkpoint(usize);

pub struct SymTable<T> {
    table: HashMap<Ustr, T>,
    undo_actions: Vec<(Ustr, Option<T>)>,
}

impl<T> SymTable<T> {
    pub fn empty() -> Self {
        Self {
            table: HashMap::new(),
            undo_actions: Vec::new(),
        }
    }

    pub fn get(&self, name: &Ustr) -> Option<&T> {
        self.table.get(name)
    }

    pub fn insert(&mut self, name: Ustr, value: T) {
        let old = self.table.insert(name, value);
        self.undo_actions.push((name, old));
    }

    pub fn checkpoint(&self) -> Checkpoint {
        Checkpoint(self.undo_actions.len())
    }

    pub fn return_to(&mut self, c: Checkpoint) {
        for _ in c.0..self.undo_actions.len() {
            let (name, val_option) = self.undo_actions.pop().unwrap();
            match val_option {
                Some(v) => self.table.insert(name, v),
                None => self.table.remove(&name),
            };
        }
    }
}
