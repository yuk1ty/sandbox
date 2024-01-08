use std::{
    ops::Deref,
    ptr::NonNull,
    sync::atomic::{self, AtomicUsize, Ordering},
};

struct ArcData<T> {
    ref_count: AtomicUsize,
    data: T,
}

pub struct Arc<T> {
    ptr: NonNull<ArcData<T>>,
}

impl<T> Arc<T> {
    pub fn new(data: T) -> Arc<T> {
        Self {
            // Box::leakは確保した領域への排他的な所有権を放棄する。
            ptr: NonNull::from(Box::leak(Box::new(ArcData {
                ref_count: AtomicUsize::new(1),
                data,
            }))),
        }
    }

    fn data(&self) -> &ArcData<T> {
        unsafe { self.ptr.as_ref() }
    }

    // DerefMutのように、無条件に&mut
    // Tを約束するわけには行かないが、一方で参照カウントが1だったときかつ、
    // 他のArcオブジェクトが他のデータにアクセスしないと保証できれば、&mut Tを返すメソッドを定義することができる。
    pub fn get_mut(arc: &mut Self) -> Option<&mut T> {
        if arc.data().ref_count.load(Ordering::Relaxed) == 1 {
            atomic::fence(Ordering::Acquire);
            unsafe { Some(&mut arc.ptr.as_mut().data) }
        } else {
            None
        }
    }
}

// DerefMutは実装しない。これは、`Arc<T>`は共有所有を表すため。&mut
// Tを実装してしまうと、実質的に排他所有を無条件に扱えることになってしまう。
impl<T> Deref for Arc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.data().data
    }
}

impl<T> Clone for Arc<T> {
    fn clone(&self) -> Self {
        if self.data().ref_count.fetch_add(1, Ordering::Relaxed) > usize::MAX / 2 {
            std::process::abort();
        }
        Self { ptr: self.ptr }
    }
}

impl<T> Drop for Arc<T> {
    fn drop(&mut self) {
        if self.data().ref_count.fetch_sub(1, Ordering::Release) == 1 {
            atomic::fence(Ordering::Acquire);
            unsafe {
                drop(Box::from_raw(self.ptr.as_ptr()));
            }
        }
    }
}

unsafe impl<T: Send + Sync> Send for Arc<T> {}
unsafe impl<T: Send + Sync> Sync for Arc<T> {}

fn main() {}

#[cfg(test)]
mod tests {
    use loom::lazy_static::Lazy;
    use loom::sync::atomic::AtomicUsize;
    use loom::sync::atomic::Ordering::{Acquire, Relaxed, Release, SeqCst};
    use loom::thread;
    use std::marker::PhantomData;

    use crate::Arc;

    #[test]
    fn test() {
        // static NUM_DROPS: AtomicUsize = AtomicUsize::new(0);
        static NUM_DROPS: Lazy<AtomicUsize> = Lazy {
            init: || AtomicUsize::new(0),
            _p: PhantomData,
        };

        struct DetectDrop;

        impl Drop for DetectDrop {
            fn drop(&mut self) {
                NUM_DROPS.get().fetch_add(1, Relaxed);
            }
        }

        loom::model(|| {
            // Create two Arcs sharing an object containing a string
            // and a DetectDrop, to detect when it's dropped.
            let x = Arc::new(("hello", DetectDrop));
            let y = x.clone();

            // Send x to another thread, and use it there.
            let t = thread::spawn(move || {
                assert_eq!(x.0, "hello");
            });

            // In parallel, y should still be usable here.
            assert_eq!(y.0, "hello");

            // Wait for the thread to finish.
            t.join().unwrap();

            // One Arc, x, should be dropped by now.
            // We still have y, so the object shouldn't have been dropped yet.
            assert_eq!(NUM_DROPS.get().load(Relaxed), 0);

            // Drop the remaining `Arc`.
            drop(y);

            // Now that `y` is dropped too,
            // the object should've been dropped.
            assert_eq!(NUM_DROPS.get().load(Relaxed), 1);
        });
    }

    #[test]
    #[should_panic]
    fn test_concurrent_logic_1() {
        loom::model(|| {
            let v1 = Arc::new(AtomicUsize::new(0));
            let v2 = v1.clone();
            thread::spawn(move || {
                v1.store(1, SeqCst);
            });

            assert_eq!(0, v2.load(SeqCst));
        });
    }

    #[test]
    #[should_panic]
    fn test_concurrent_logic_2() {
        loom::model(|| {
            let num = Arc::new(AtomicUsize::new(0));

            let ths: Vec<_> = (0..2)
                .map(|_| {
                    let num = num.clone();
                    thread::spawn(move || {
                        let curr = num.load(Acquire);
                        num.store(curr + 1, Release);
                    })
                })
                .collect();

            for th in ths {
                th.join().unwrap();
            }

            assert_eq!(2, num.load(Relaxed));
        });
    }
}
