// use ghost_cell::{GhostCell, GhostToken};
// use static_rc::StaticRc;

// pub type HalfNode<'id, T> = StaticRc<GhostCell<'id, Node<'id, T>>, 1, 2>;
// pub type FullNode<'id, T> = StaticRc<GhostCell<'id, Node<'id, T>>, 2, 2>;
// pub type HalfLink<'id, T> = Option<HalfNode<'id, T>>;
// pub type FullLink<'id, T> = Option<HalfNode<'id, T>>;

// #[derive(Debug)]
// pub struct DoublyLinkedList<'id, T> {
//     pub head: HalfLink<'id, T>,
//     pub tail: HalfLink<'id, T>,
//     pub len: usize,
// }

// use std::fmt::Debug;

// impl<'id, T: Debug> DoublyLinkedList<'id, T> {
//     pub fn new() -> Self {
//         Self {
//             head: None,
//             tail: None,
//             len: 0,
//         }
//     }

//     pub fn push_front(&mut self, value: T, token: &mut GhostToken<'id>) {
//         let new_head = StaticRc::new(GhostCell::new(Node::new(value)));
//         let (itself, cloned) = StaticRc::split::<1, 1>(new_head);
//         match self.head.take() {
//             Some(old_head) => {
//                 old_head.borrow_mut(token).prev = Some(itself);
//                 cloned.borrow_mut(token).next = Some(old_head);
//                 self.head = Some(cloned);
//                 self.len += 1;
//             }
//             None => {
//                 self.head = Some(itself);
//                 self.tail = Some(cloned);
//                 self.len = 1;
//             }
//         }
//     }

//     pub fn push_back(&mut self, value: T, token: &mut GhostToken<'id>) {
//         let new_tail = StaticRc::new(GhostCell::new(Node::new(value)));
//         let (itself, cloned) = StaticRc::split::<1, 1>(new_tail);
//         match self.tail.take() {
//             Some(old_tail) => {
//                 old_tail.borrow_mut(token).next = Some(itself);
//                 cloned.borrow_mut(token).prev = Some(old_tail);
//                 self.tail = Some(cloned);
//                 self.len += 1;
//             }
//             None => {
//                 self.head = Some(itself);
//                 self.tail = Some(cloned);
//                 self.len = 1;
//             }
//         }
//     }

//     pub fn pop_back(&mut self, token: &mut GhostToken<'id>) -> Option<T> {
//         self.tail.take().map(|old_tail| {
//             match old_tail.borrow_mut(token).prev.take() {
//                 Some(new_tail) => {
//                     new_tail.borrow_mut(token).next.take();
//                     self.tail = Some(new_tail);
//                 }
//                 None => {
//                     self.head = None;
//                 }
//             }
//             let full = StaticRc::adjust::<2, 2>(old_tail);
//             let ghost_cell = StaticRc::into_inner(full);
//             let node = GhostCell::into_inner(ghost_cell);
//             node.data
//         })
//     }

//     pub fn clear(&mut self, token: &mut GhostToken<'id>) {
//         while self.pop_back(token).is_some() {}
//     }
// }

// #[derive(Debug)]
// pub struct Node<'id, T> {
//     pub data: T,
//     pub prev: HalfLink<'id, T>,
//     pub next: HalfLink<'id, T>,
// }

// impl<'id, T> Node<'id, T> {
//     pub fn new(value: T) -> Node<'id, T> {
//         let base = Node {
//             data: value,
//             prev: None,
//             next: None,
//         };
//         base
//     }
// }

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::DoublyLinkedList;
//     use ghost_cell::GhostToken;

//     #[test]
//     fn create_node() {
//         GhostToken::new(|mut token| {
//             let mut node = Node::new(1);
//             node.next = Some(StaticRc::new(GhostCell::new(Node::new(2))));
//         });
//     }

//     #[test]
//     fn empty_pop_back() {
//         GhostToken::new(|mut token| {
//             let mut list: DoublyLinkedList<i32> = DoublyLinkedList::new();
//             let pop_back = list.pop_back(&mut token);
//             assert_eq!(pop_back, None);
//         });
//     }

//     #[test]
//     fn push_back() {
//         GhostToken::new(|mut token| {
//             let mut list = DoublyLinkedList::new();
//             list.push_back(1, &mut token);
//             println!("{:?}", list);
//         });
//     }

//     // TODO なんか落ちるw
//     #[test]
//     fn add_sequential_numbers_to_existing_list_and_get_four_elements() {
//         GhostToken::new(|mut token| {
//             let mut list = DoublyLinkedList::new();
//             list.push_front(1, &mut token);
//             list.push_front(2, &mut token);
//             list.push_front(3, &mut token);
//             list.push_front(4, &mut token);

//             assert_eq!(list.len, 4);
//             assert_eq!(list.pop_back(&mut token).unwrap(), 4);
//             assert_eq!(list.pop_back(&mut token).unwrap(), 3);
//             assert_eq!(list.pop_back(&mut token).unwrap(), 2);
//             assert_eq!(list.pop_back(&mut token).unwrap(), 1);

//             list.clear(&mut token);
//         });
//     }
// }

fn main() {
    println!("Hello, world!");
}
