#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
fn main() {
    let body = async {
        {
            #[doc(hidden)]
            mod __tokio_select_util {
                pub(super) enum Out<_0, _1> {
                    _0(_0),
                    _1(_1),
                    Disabled,
                }
                pub(super) type Mask = u8;
            }
            use ::tokio::macros::support::Future;
            use ::tokio::macros::support::Pin;
            use ::tokio::macros::support::Poll::{Ready, Pending};
            const BRANCHES: u32 = 2;
            let mut disabled: __tokio_select_util::Mask = Default::default();
            if !true {
                let mask: __tokio_select_util::Mask = 1 << 0;
                disabled |= mask;
            }
            if !true {
                let mask: __tokio_select_util::Mask = 1 << 1;
                disabled |= mask;
            }
            let mut output = {
                let mut futures = (
                    tokio::spawn(
                        {
                            ::async_backtrace::Location::from_components(
                                {
                                    fn type_name_of_val<T: ?Sized>(_: &T) -> &'static str {
                                        core::any::type_name::<T>()
                                    }
                                    type_name_of_val(&(|| {}))
                                        .strip_suffix("::{{closure}}")
                                        .unwrap()
                                },
                                &("src/main.rs", 4u32, 26u32),
                            )
                        }
                            .frame(pending()),
                    ),
                    foo(),
                );
                ::tokio::macros::support::poll_fn(|cx| {
                        let mut is_pending = false;
                        let start = { ::tokio::macros::support::thread_rng_n(BRANCHES) };
                        for i in 0..BRANCHES {
                            let branch;
                            #[allow(clippy::modulo_one)]
                            {
                                branch = (start + i) % BRANCHES;
                            }
                            match branch {
                                #[allow(unreachable_code)]
                                0 => {
                                    let mask = 1 << branch;
                                    if disabled & mask == mask {
                                        continue;
                                    }
                                    let (fut, ..) = &mut futures;
                                    let mut fut = unsafe { Pin::new_unchecked(fut) };
                                    let out = match Future::poll(fut, cx) {
                                        Ready(out) => out,
                                        Pending => {
                                            is_pending = true;
                                            continue;
                                        }
                                    };
                                    disabled |= mask;
                                    #[allow(unused_variables)] #[allow(unused_mut)]
                                    match &out {
                                        _ => {}
                                        _ => continue,
                                    }
                                    return Ready(__tokio_select_util::Out::_0(out));
                                }
                                #[allow(unreachable_code)]
                                1 => {
                                    let mask = 1 << branch;
                                    if disabled & mask == mask {
                                        continue;
                                    }
                                    let (_, fut, ..) = &mut futures;
                                    let mut fut = unsafe { Pin::new_unchecked(fut) };
                                    let out = match Future::poll(fut, cx) {
                                        Ready(out) => out,
                                        Pending => {
                                            is_pending = true;
                                            continue;
                                        }
                                    };
                                    disabled |= mask;
                                    #[allow(unused_variables)] #[allow(unused_mut)]
                                    match &out {
                                        _ => {}
                                        _ => continue,
                                    }
                                    return Ready(__tokio_select_util::Out::_1(out));
                                }
                                _ => {
                                    ::core::panicking::unreachable_display(
                                        &"reaching this means there probably is an off by one bug",
                                    )
                                }
                            }
                        }
                        if is_pending {
                            Pending
                        } else {
                            Ready(__tokio_select_util::Out::Disabled)
                        }
                    })
                    .await
            };
            match output {
                __tokio_select_util::Out::_0(_) => {}
                __tokio_select_util::Out::_1(_) => {}
                __tokio_select_util::Out::Disabled => {
                    ::std::rt::begin_panic(
                        "all branches are disabled and there is no else branch",
                    )
                }
                _ => ::core::panicking::unreachable_display(&"failed to match bind"),
            }
        };
    };
    #[allow(clippy::expect_used, clippy::diverging_sub_expression)]
    {
        return tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .expect("Failed building the Runtime")
            .block_on(body);
    }
}
async fn pending() {
    {
        ::async_backtrace::Location::from_components(
            {
                fn type_name_of_val<T: ?Sized>(_: &T) -> &'static str {
                    core::any::type_name::<T>()
                }
                type_name_of_val(&(|| {})).strip_suffix("::{{closure}}").unwrap()
            },
            &("src/main.rs", 9u32, 1u32),
        )
    }
        .frame(async move {
            {
                #[allow(
                    unreachable_code,
                    clippy::diverging_sub_expression,
                    clippy::let_unit_value
                )]
                if false {
                    let __backtrace_attr_fake_return: () = loop {};
                    return __backtrace_attr_fake_return;
                }
                { std::future::pending::<()>().await }
            }
        })
        .await
}
async fn foo() {
    {
        ::async_backtrace::Location::from_components(
            {
                fn type_name_of_val<T: ?Sized>(_: &T) -> &'static str {
                    core::any::type_name::<T>()
                }
                type_name_of_val(&(|| {})).strip_suffix("::{{closure}}").unwrap()
            },
            &("src/main.rs", 14u32, 1u32),
        )
    }
        .frame(async move {
            {
                #[allow(
                    unreachable_code,
                    clippy::diverging_sub_expression,
                    clippy::let_unit_value
                )]
                if false {
                    let __backtrace_attr_fake_return: () = loop {};
                    return __backtrace_attr_fake_return;
                }
                {
                    bar().await;
                }
            }
        })
        .await
}
async fn bar() {
    {
        ::async_backtrace::Location::from_components(
            {
                fn type_name_of_val<T: ?Sized>(_: &T) -> &'static str {
                    core::any::type_name::<T>()
                }
                type_name_of_val(&(|| {})).strip_suffix("::{{closure}}").unwrap()
            },
            &("src/main.rs", 19u32, 1u32),
        )
    }
        .frame(async move {
            {
                #[allow(
                    unreachable_code,
                    clippy::diverging_sub_expression,
                    clippy::let_unit_value
                )]
                if false {
                    let __backtrace_attr_fake_return: () = loop {};
                    return __backtrace_attr_fake_return;
                }
                {
                    {
                        use ::futures_util::__private as __futures_crate;
                        {
                            let mut _fut0 = __futures_crate::future::maybe_done(fiz());
                            let mut _fut0 = unsafe {
                                __futures_crate::Pin::new_unchecked(&mut _fut0)
                            };
                            let mut _fut1 = __futures_crate::future::maybe_done(buz());
                            let mut _fut1 = unsafe {
                                __futures_crate::Pin::new_unchecked(&mut _fut1)
                            };
                            __futures_crate::future::poll_fn(move |
                                    __cx: &mut __futures_crate::task::Context<'_>|
                                {
                                    let mut __all_done = true;
                                    __all_done
                                        &= __futures_crate::future::Future::poll(
                                                _fut0.as_mut(),
                                                __cx,
                                            )
                                            .is_ready();
                                    __all_done
                                        &= __futures_crate::future::Future::poll(
                                                _fut1.as_mut(),
                                                __cx,
                                            )
                                            .is_ready();
                                    if __all_done {
                                        __futures_crate::task::Poll::Ready((
                                            _fut0.as_mut().take_output().unwrap(),
                                            _fut1.as_mut().take_output().unwrap(),
                                        ))
                                    } else {
                                        __futures_crate::task::Poll::Pending
                                    }
                                })
                                .await
                        }
                    };
                }
            }
        })
        .await
}
async fn fiz() {
    {
        ::async_backtrace::Location::from_components(
            {
                fn type_name_of_val<T: ?Sized>(_: &T) -> &'static str {
                    core::any::type_name::<T>()
                }
                type_name_of_val(&(|| {})).strip_suffix("::{{closure}}").unwrap()
            },
            &("src/main.rs", 24u32, 1u32),
        )
    }
        .frame(async move {
            {
                #[allow(
                    unreachable_code,
                    clippy::diverging_sub_expression,
                    clippy::let_unit_value
                )]
                if false {
                    let __backtrace_attr_fake_return: () = loop {};
                    return __backtrace_attr_fake_return;
                }
                {
                    tokio::task::yield_now().await;
                }
            }
        })
        .await
}
async fn buz() {
    {
        ::std::io::_print(
            ::core::fmt::Arguments::new_v1(
                &["", "\n"],
                &[::core::fmt::ArgumentV1::new_display(&baz().await)],
            ),
        );
    };
}
async fn baz() -> String {
    {
        ::async_backtrace::Location::from_components(
            {
                fn type_name_of_val<T: ?Sized>(_: &T) -> &'static str {
                    core::any::type_name::<T>()
                }
                type_name_of_val(&(|| {})).strip_suffix("::{{closure}}").unwrap()
            },
            &("src/main.rs", 34u32, 1u32),
        )
    }
        .frame(async move {
            {
                #[allow(
                    unreachable_code,
                    clippy::diverging_sub_expression,
                    clippy::let_unit_value
                )]
                if false {
                    let __backtrace_attr_fake_return: String = loop {};
                    return __backtrace_attr_fake_return;
                }
                { async_backtrace::taskdump_tree(true) }
            }
        })
        .await
}
