use std::sync::Mutex;

pub struct Counter {
    count: Mutex<usize>,
    prefix: String,
}

impl Counter{
    pub fn new(prefix: &str) -> Self {
        Counter { count: Mutex::new(0), prefix: prefix.to_string()}
    }

    pub fn reset(&self) {
        *self.count.lock().unwrap() = 0;
    }

    pub fn new_id(&self) -> String {
      let mut count = self.count.lock().unwrap();
      let id = format!("{}{}", self.prefix, *count);
      *count += 1;
      id
    }
}