use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone)]
pub struct TokenQueue<T: Clone> {
    contents: Rc<RefCell<Vec<T>>>,
    next: usize,
}

impl<T: Clone> TokenQueue<T> {
    pub fn new() -> TokenQueue<T> {
        TokenQueue {
            contents: Rc::new(RefCell::new(Vec::new())),
            next: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.next == self.contents.borrow().len()
    }

    pub fn peek(&self) -> Option<T> {
        self.contents.borrow().get(self.next).map(T::clone)
    }

    pub fn peek_map<U>(&self, f: impl FnOnce(&T) -> U) -> Option<U> {
        self.contents.borrow().get(self.next).map(f)
    }

    pub fn push(&self, value: T) {
        self.contents.borrow_mut().push(value);
    }

    pub fn pop(&self) -> TokenQueue<T> {
        assert!(!self.is_empty());
        TokenQueue {
            contents: Rc::clone(&self.contents),
            next: self.next + 1,
        }
    }
}
