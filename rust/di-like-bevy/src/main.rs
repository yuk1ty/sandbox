use std::{
    any::{Any, TypeId},
    collections::HashMap,
    marker::PhantomData,
};

struct Scheduler {
    systems: Vec<StoredSystem>,
    // TODO: Maybe we can use anymap::Map here!
    resources: HashMap<TypeId, Box<dyn Any>>,
}

impl Scheduler {
    pub fn run(&mut self) {
        for system in self.systems.iter_mut() {
            system.run(&mut self.resources);
        }
    }

    pub fn add_system<I, S: System + 'static>(&mut self, system: impl IntoSystem<I, System = S>) {
        self.systems.push(Box::new(system.into_system()));
    }

    pub fn add_resource<R: 'static>(&mut self, res: R) {
        self.resources.insert(TypeId::of::<R>(), Box::new(res));
    }
}

trait System {
    fn run(&mut self, resources: &mut HashMap<TypeId, Box<dyn Any>>);
}

macro_rules! impl_system {
    (
        $(
            $($params:ident),+
        )?
    ) => {
        #[allow(non_snake_case, unused)]
        impl<
            F: FnMut(
                $( $($params),+ )?
            )
            $(, $($params: 'static),+ )?
        > System for FunctionSystem<($( $($params,)+ )?), F> {
            fn run(&mut self, resources: &mut HashMap<TypeId, Box<dyn Any>>) {
                $($(
                    let $params = *resources.remove(&TypeId::of::<$params>()).unwrap().downcast::<$params>().unwrap();
                )+)?

                (self.f)(
                    $($($params),+)?
                );
            }
        }
    }
}

impl_system!();
impl_system!(T1);
impl_system!(T1, T2);
impl_system!(T1, T2, T3);
impl_system!(T1, T2, T3, T4);

type StoredSystem = Box<dyn System>;

struct FunctionSystem<Input, F> {
    f: F,
    marker: PhantomData<fn() -> Input>,
}

trait IntoSystem<Input> {
    type System: System;
    fn into_system(self) -> Self::System;
}

macro_rules! impl_into_system {
    (
        $($(
                $params:ident
        ),+)?
    ) => {
        impl<F: FnMut($($($params),+)?) $(, $($params: 'static),+ )?> IntoSystem<( $($($params,)+)? )> for F {
            type System = FunctionSystem<( $($($params,)+)? ), Self>;

            fn into_system(self) -> Self::System {
                FunctionSystem {
                    f: self,
                    marker: Default::default(),
                }
            }
        }
    }
}

impl_into_system!();
impl_into_system!(T1);
impl_into_system!(T1, T2);
impl_into_system!(T1, T2, T3);
impl_into_system!(T1, T2, T3, T4);

trait SystemParam<'a> {
    fn retrieve(resources: &'a mut HashMap<TypeId, Box<dyn Any>>) -> Self;
}

struct Res<'a, T: 'static> {
    value: &'a T,
}

impl<'a, T: 'static> SystemParam<'a> for Res<'a, T> {
    fn retrieve(resources: &'a mut HashMap<TypeId, Box<dyn Any>>) -> Self {
        let value = resources
            .get_mut(&TypeId::of::<T>())
            .unwrap()
            .downcast_ref::<T>()
            .unwrap();
        Res { value }
    }
}

fn main() {
    let mut scheduler = Scheduler {
        systems: Vec::new(),
        resources: HashMap::default(),
    };

    scheduler.add_system(foo);
    scheduler.add_resource(12i32);

    scheduler.run();
}

fn foo(int: i32) {
    println!("int! {int}");
}
