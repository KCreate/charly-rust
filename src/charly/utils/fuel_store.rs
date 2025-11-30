// MIT License
//
// Copyright (c) 2025 Leonard Schütz
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

pub struct FuelStore {
    name: String,
    fuel: i32,
    initial_capacity: i32,
}

impl FuelStore {
    pub fn new(name: &str, initial_capacity: i32) -> FuelStore {
        Self {
            name: name.to_string(),
            fuel: initial_capacity,
            initial_capacity,
        }
    }

    pub fn consume(&mut self) {
        self.fuel = self.fuel - 1;
        if self.fuel <= 0 {
            panic!("{}: ran out of fuel!", self.name)
        }
    }

    pub fn replenish(&mut self) {
        self.fuel = self.initial_capacity;
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_fuel_store() {
        let mut store = super::FuelStore::new("test", 3);
        store.consume();
        store.consume();
        store.replenish();
        store.consume();
        store.consume();
        store.replenish();
    }

    #[test]
    #[should_panic(expected = "test: ran out of fuel!")]
    fn test_fuel_store_out_of_fuel() {
        let mut store = super::FuelStore::new("test", 2);
        store.consume();
        store.consume();
    }
}
