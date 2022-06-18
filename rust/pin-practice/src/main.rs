#![feature(auto_traits, negative_impls, generator_trait)]
use std::pin::Pin;

enum GeneratorState<Y, R> {
    Yielded(Y),
    Complete(R),
}

trait Generator {
    type Yield;
    type Return;
    fn resume(self: Pin<&mut Self>) -> GeneratorState<Self::Yield, Self::Return>;
}

enum GeneratorA {
    Enter,
    Yield1 {
        to_borrow: String,
        borrowed: *const String,
    },
    Exit,
}

impl GeneratorA {
    fn start() -> Self {
        GeneratorA::Enter
    }
}

impl !Unpin for GeneratorA {}

impl Generator for GeneratorA {
    type Yield = usize;
    type Return = ();

    fn resume(self: Pin<&mut Self>) -> GeneratorState<Self::Yield, Self::Return> {
        let this = unsafe { self.get_unchecked_mut() };
        match this {
            GeneratorA::Enter => {
                let to_borrow = String::from("Hello");
                let borrowed = &to_borrow;
                let res = borrowed.len();
                *this = GeneratorA::Yield1 {
                    to_borrow,
                    borrowed: std::ptr::null(),
                };

                if let GeneratorA::Yield1 {
                    to_borrow,
                    borrowed,
                } = this
                {
                    *borrowed = to_borrow;
                }

                GeneratorState::Yielded(res)
            }
            GeneratorA::Yield1 { borrowed, .. } => {
                let borrowed: &String = unsafe { &**borrowed };
                println!("{} world", borrowed);
                *this = GeneratorA::Exit;
                GeneratorState::Complete(())
            }
            GeneratorA::Exit => panic!("Can't advance an exited generator!"),
        }
    }
}

fn main() {
    let gen1 = GeneratorA::start();
    let gen2 = GeneratorA::start();

    let mut pinned1 = Box::pin(gen1);
    let mut pinned2 = Box::pin(gen2);

    if let GeneratorState::Yielded(n) = pinned1.as_mut().resume() {
        println!("Gen1 got value {}", n);
    }

    if let GeneratorState::Yielded(n) = pinned2.as_mut().resume() {
        println!("Gen2 got value {}", n);
    }
}
