use std::rc::Rc;

struct Node {
    val: i32,
    points_to: Option<Rc<Node>>,
}

fn main() {
    let n3 = Rc::new(Node {
        val: 3,
        points_to: None,
    });
    dbg!(Rc::strong_count(&n3));
    let n2 = Node {
        val: 2,
        points_to: Some(Rc::clone(&n3)),
    };
    dbg!(Rc::strong_count(&n3));
    let n1 = Node {
        val: 1,
        points_to: Some(Rc::clone(&n3)),
    };
    dbg!(Rc::strong_count(&n3));
}
