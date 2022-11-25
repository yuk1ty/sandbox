trait StreamingIterator {
    type Item<'a>
    where
        Self: 'a;
    fn next(&mut self) -> Option<Self::Item<'_>>;
}

struct VecWrapper<T> {
    inner: Vec<T>,
    cur: usize,
}

impl<T> StreamingIterator for VecWrapper<T> {
    type Item<'a> = &'a mut T where Self: 'a;
    fn next(&mut self) -> Option<Self::Item<'_>> {
        self.inner.get_mut(self.cur).map_or(None, |item| {
            self.cur += 1;
            Some(item)
        })
    }
}

// use std::marker::PhantomData;

// NG
// trait StreamingIterator {
//     type Item;
//     fn next(&mut self) -> Option<Self::Item>;
// }

// struct VecWrapper<'a, T> {
//     inner: Vec<T>,
//     cur: usize,
//     _marker: PhantomData<&'a T>,
// }

// NG
// error: lifetime may not live long enough
//   --> src/bin/gats.rs:36:9
//    |
// 33 |   impl<'a, T> StreamingIterator for VecWrapper<'a, T> {
//    |        -- lifetime `'a` defined here
// 34 |       type Item = &'a mut T;
// 35 |       fn next(&mut self) -> Option<Self::Item> {
//    |               - let's call the lifetime of this reference `'1`
// 36 | /         self.inner.get_mut(self.cur).map_or(None, |item| {
// 37 | |             self.cur += 1;
// 38 | |             Some(item)
// 39 | |         })
//    | |__________^ associated function was supposed to return data with lifetime `'a` but it is returning data with lifetime `'1`
// impl<'a, T> StreamingIterator for VecWrapper<'a, T> {
//     type Item = &'a mut T;
//     fn next(&mut self) -> Option<Self::Item> {
//         self.inner.get_mut(self.cur).map_or(None, |item| {
//             self.cur += 1;
//             Some(item)
//         })
//     }
// }

fn main() {}
