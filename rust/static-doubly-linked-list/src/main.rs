use ghost_cell::{GhostCell, GhostToken};
use static_rc::StaticRc;

pub type HalfNode<'id, T> = StaticRc<GhostCell<'id, Node<'id, T>>, 1, 2>;
pub type FullNode<'id, T> = StaticRc<GhostCell<'id, Node<'id, T>>, 2, 2>;
pub type HalfLink<'id, T> = Option<HalfNode<'id, T>>;
pub type FullLink<'id, T> = Option<HalfNode<'id, T>>;

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

    pub fn push_front(&mut self, value: T, token: &mut GhostToken<'id>) {
        let new_head = Node::new(value);
        let (itself, cloned) = StaticRc::split::<1, 1>(new_head);
        match self.head.take() {
            Some(old_head) => {
                old_head.borrow_mut(token).prev = Some(itself);
                cloned.borrow_mut(token).next = Some(old_head);
                self.head = Some(cloned);
                self.tail = self.tail.take();
                self.len += 1;
            }
            None => {
                self.head = Some(itself);
                self.tail = Some(cloned);
                self.len = 1;
            }
        }
    }

    pub fn push_back(&mut self, value: T, token: &mut GhostToken<'id>) {
        let new_tail = Node::new(value);
        let (itself, cloned) = StaticRc::split::<1, 1>(new_tail);
        match self.tail.take() {
            Some(old_tail) => {
                old_tail.borrow_mut(token).next = Some(itself);
                cloned.borrow_mut(token).prev = Some(old_tail);
                self.tail = Some(cloned);
                self.len += 1;
            }
            None => {
                self.head = Some(itself);
                self.tail = Some(cloned);
                self.len = 1;
            }
        }
    }
}

pub struct Node<'id, T> {
    pub data: T,
    pub prev: HalfLink<'id, T>,
    pub next: HalfLink<'id, T>,
}

impl<'id, T> Node<'id, T> {
    pub fn new(value: T) -> FullNode<'id, T> {
        let base = Node {
            data: value,
            prev: None,
            next: None,
        };
        let full_node = StaticRc::new(GhostCell::new(base));
        full_node
    }
}

#[cfg(test)]
mod tests {
    use ghost_cell::GhostToken;

    use crate::DoublyLinkedList;

    // TODO なんか落ちるw
    #[test]
    fn add_sequential_numbers_to_existing_list_and_get_four_elements() {
        GhostToken::new(|mut token| {
            let mut list = DoublyLinkedList::new();
            list.push_front(1, &mut token);
            list.push_front(2, &mut token);
            list.push_front(3, &mut token);
            list.push_front(4, &mut token);
            assert_eq!(list.len, 4);
        });
    }
}

fn main() {
    println!("Hello, world!");
}
