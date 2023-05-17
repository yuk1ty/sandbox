use ghost_cell::GhostCell;
use static_rc::StaticRc;

pub type HalfLink<'id, T> = Option<StaticRc<GhostCell<'id, Node<'id, T>>, 1, 2>>;
// pub type FullLink<'id, T> = Option<StaticRc<GhostCell<'id, Node<'id, T>>, 2, 2>>;

pub struct DoublyLinkedList<'id, T> {
    pub head: HalfLink<'id, T>,
    pub tail: HalfLink<'id, T>,
    pub len: usize,
}

impl<'id, T> DoublyLinkedList<'id, T> {
    pub fn new() -> Self {
        Self {
            head: None,
            tail: None,
            len: 0,
        }
    }
}

pub struct Node<'id, T> {
    pub data: T,
    pub prev: HalfLink<'id, T>,
    pub next: HalfLink<'id, T>,
}

fn main() {
    println!("Hello, world!");
}
