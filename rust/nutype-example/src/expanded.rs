#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
use std::marker::PhantomData;
use currency::Currency;
use nutype::nutype;
pub struct Stock<C>
where
    C: Currency,
{
    stock_symbol: TickerSymbol,
    name: String,
    last_sale: Price<C>,
    change_rate: Percent,
    market_cap: u128,
}
#[automatically_derived]
impl<C: ::core::fmt::Debug> ::core::fmt::Debug for Stock<C>
where
    C: Currency,
{
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field5_finish(
            f,
            "Stock",
            "stock_symbol",
            &self.stock_symbol,
            "name",
            &self.name,
            "last_sale",
            &self.last_sale,
            "change_rate",
            &self.change_rate,
            "market_cap",
            &&self.market_cap,
        )
    }
}
#[automatically_derived]
impl<C: ::core::clone::Clone> ::core::clone::Clone for Stock<C>
where
    C: Currency,
{
    #[inline]
    fn clone(&self) -> Stock<C> {
        Stock {
            stock_symbol: ::core::clone::Clone::clone(&self.stock_symbol),
            name: ::core::clone::Clone::clone(&self.name),
            last_sale: ::core::clone::Clone::clone(&self.last_sale),
            change_rate: ::core::clone::Clone::clone(&self.change_rate),
            market_cap: ::core::clone::Clone::clone(&self.market_cap),
        }
    }
}
#[doc(hidden)]
mod __nutype_private_TickerSymbol__ {
    use super::*;
    pub struct TickerSymbol(String);
    #[automatically_derived]
    impl ::core::clone::Clone for TickerSymbol {
        #[inline]
        fn clone(&self) -> TickerSymbol {
            TickerSymbol(::core::clone::Clone::clone(&self.0))
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for TickerSymbol {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for TickerSymbol {
        #[inline]
        fn eq(&self, other: &TickerSymbol) -> bool {
            self.0 == other.0
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Ord for TickerSymbol {
        #[inline]
        fn cmp(&self, other: &TickerSymbol) -> ::core::cmp::Ordering {
            ::core::cmp::Ord::cmp(&self.0, &other.0)
        }
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for TickerSymbol {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "TickerSymbol",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralEq for TickerSymbol {}
    #[automatically_derived]
    impl ::core::cmp::Eq for TickerSymbol {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<String>;
        }
    }
    #[automatically_derived]
    impl ::core::cmp::PartialOrd for TickerSymbol {
        #[inline]
        fn partial_cmp(
            &self,
            other: &TickerSymbol,
        ) -> ::core::option::Option<::core::cmp::Ordering> {
            ::core::cmp::PartialOrd::partial_cmp(&self.0, &other.0)
        }
    }
    #[automatically_derived]
    impl ::core::hash::Hash for TickerSymbol {
        #[inline]
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
            ::core::hash::Hash::hash(&self.0, state)
        }
    }
    pub enum TickerSymbolError {
        Empty,
        TooLong,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for TickerSymbolError {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(
                f,
                match self {
                    TickerSymbolError::Empty => "Empty",
                    TickerSymbolError::TooLong => "TooLong",
                },
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for TickerSymbolError {
        #[inline]
        fn clone(&self) -> TickerSymbolError {
            match self {
                TickerSymbolError::Empty => TickerSymbolError::Empty,
                TickerSymbolError::TooLong => TickerSymbolError::TooLong,
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for TickerSymbolError {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for TickerSymbolError {
        #[inline]
        fn eq(&self, other: &TickerSymbolError) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralEq for TickerSymbolError {}
    #[automatically_derived]
    impl ::core::cmp::Eq for TickerSymbolError {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {}
    }
    impl ::core::fmt::Display for TickerSymbolError {
        fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
            match self {
                TickerSymbolError::Empty => f.write_fmt(format_args!("empty")),
                TickerSymbolError::TooLong => f.write_fmt(format_args!("too long")),
            }
        }
    }
    impl ::std::error::Error for TickerSymbolError {
        fn source(&self) -> Option<&(dyn ::std::error::Error + 'static)> {
            None
        }
    }
    impl TickerSymbol {
        pub fn new(
            raw_value: impl Into<String>,
        ) -> ::core::result::Result<Self, TickerSymbolError> {
            fn sanitize(value: String) -> String {
                let value: String = value.trim().to_string();
                let value: String = value.to_uppercase();
                value
            }
            fn validate(val: &str) -> ::core::result::Result<(), TickerSymbolError> {
                let chars_count = val.chars().count();
                if val.is_empty() {
                    return Err(TickerSymbolError::Empty);
                }
                if chars_count > 5usize {
                    return Err(TickerSymbolError::TooLong);
                }
                Ok(())
            }
            let sanitized_value = sanitize(raw_value.into());
            validate(&sanitized_value)?;
            Ok(TickerSymbol(sanitized_value))
        }
    }
    impl TickerSymbol {
        pub fn into_inner(self) -> String {
            self.0
        }
    }
    impl ::core::convert::AsRef<str> for TickerSymbol {
        fn as_ref(&self) -> &str {
            &self.0
        }
    }
    impl ::core::convert::TryFrom<String> for TickerSymbol {
        type Error = TickerSymbolError;
        fn try_from(raw_value: String) -> Result<TickerSymbol, Self::Error> {
            Self::new(raw_value)
        }
    }
    impl ::core::convert::TryFrom<&str> for TickerSymbol {
        type Error = TickerSymbolError;
        fn try_from(raw_value: &str) -> Result<TickerSymbol, Self::Error> {
            Self::new(raw_value)
        }
    }
    impl core::str::FromStr for TickerSymbol {
        type Err = TickerSymbolError;
        fn from_str(raw_string: &str) -> ::core::result::Result<Self, Self::Err> {
            TickerSymbol::new(raw_string)
        }
    }
}
pub use __nutype_private_TickerSymbol__::TickerSymbol;
pub use __nutype_private_TickerSymbol__::TickerSymbolError;
mod currency {
    pub struct Usd;
    impl Currency for Usd {}
    pub struct Jpy;
    impl Currency for Jpy {}
    pub trait Currency {}
}
pub struct Price<C: Currency>(f32, PhantomData<C>);
#[automatically_derived]
impl<C: ::core::fmt::Debug + Currency> ::core::fmt::Debug for Price<C> {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_tuple_field2_finish(f, "Price", &self.0, &&self.1)
    }
}
#[automatically_derived]
impl<C: ::core::clone::Clone + Currency> ::core::clone::Clone for Price<C> {
    #[inline]
    fn clone(&self) -> Price<C> {
        Price(::core::clone::Clone::clone(&self.0), ::core::clone::Clone::clone(&self.1))
    }
}
#[doc(hidden)]
mod __nutype_private_Percent__ {
    use super::*;
    pub struct Percent(f32);
    #[automatically_derived]
    impl ::core::clone::Clone for Percent {
        #[inline]
        fn clone(&self) -> Percent {
            Percent(::core::clone::Clone::clone(&self.0))
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for Percent {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for Percent {
        #[inline]
        fn eq(&self, other: &Percent) -> bool {
            self.0 == other.0
        }
    }
    #[automatically_derived]
    impl ::core::cmp::PartialOrd for Percent {
        #[inline]
        fn partial_cmp(
            &self,
            other: &Percent,
        ) -> ::core::option::Option<::core::cmp::Ordering> {
            ::core::cmp::PartialOrd::partial_cmp(&self.0, &other.0)
        }
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Percent {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Percent", &&self.0)
        }
    }
    pub enum PercentError {
        NotFinite,
        TooSmall,
        TooBig,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for PercentError {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(
                f,
                match self {
                    PercentError::NotFinite => "NotFinite",
                    PercentError::TooSmall => "TooSmall",
                    PercentError::TooBig => "TooBig",
                },
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for PercentError {
        #[inline]
        fn clone(&self) -> PercentError {
            match self {
                PercentError::NotFinite => PercentError::NotFinite,
                PercentError::TooSmall => PercentError::TooSmall,
                PercentError::TooBig => PercentError::TooBig,
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for PercentError {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for PercentError {
        #[inline]
        fn eq(&self, other: &PercentError) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralEq for PercentError {}
    #[automatically_derived]
    impl ::core::cmp::Eq for PercentError {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {}
    }
    impl ::core::fmt::Display for PercentError {
        fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
            match self {
                PercentError::NotFinite => f.write_fmt(format_args!("not finite")),
                PercentError::TooSmall => f.write_fmt(format_args!("too small")),
                PercentError::TooBig => f.write_fmt(format_args!("too big")),
            }
        }
    }
    impl ::std::error::Error for PercentError {
        fn source(&self) -> Option<&(dyn ::std::error::Error + 'static)> {
            None
        }
    }
    impl Percent {
        pub fn new(raw_value: f32) -> ::core::result::Result<Self, PercentError> {
            fn sanitize(mut value: f32) -> f32 {
                value
            }
            fn validate(val: f32) -> core::result::Result<(), PercentError> {
                if !val.is_finite() {
                    return Err(PercentError::NotFinite);
                }
                if val < -100f32 {
                    return Err(PercentError::TooSmall);
                }
                if val > 100f32 {
                    return Err(PercentError::TooBig);
                }
                Ok(())
            }
            let sanitized_value = sanitize(raw_value);
            validate(sanitized_value)?;
            Ok(Percent(sanitized_value))
        }
    }
    impl Percent {
        pub fn into_inner(self) -> f32 {
            self.0
        }
    }
    #[allow(clippy::derive_ord_xor_partial_ord)]
    impl ::core::cmp::Ord for Percent {
        fn cmp(&self, other: &Self) -> ::core::cmp::Ordering {
            self.partial_cmp(other)
                .unwrap_or_else(|| {
                    let tp = "Percent";
                    {
                        ::core::panicking::panic_fmt(
                            format_args!(
                                "{0}::cmp() panicked, because partial_cmp() returned None. Could it be that you\'re using unsafe {0}::new_unchecked() ?",
                                tp
                            ),
                        );
                    };
                })
        }
    }
    impl ::core::cmp::Eq for Percent {}
}
pub use __nutype_private_Percent__::Percent;
pub use __nutype_private_Percent__::PercentError;
fn main() {}
