use std::marker::PhantomData;

use currency::Currency;
use nutype::nutype;

#[derive(Debug, Clone)]
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

#[nutype(
    sanitize(trim, uppercase)
    validate(not_empty, max_len = 5)
)]
#[derive(*)]
pub struct TickerSymbol(String);

mod currency {
    pub struct Usd;
    impl Currency for Usd {}
    pub struct Jpy;
    impl Currency for Jpy {}
    pub trait Currency {}
}

// PhantomData 込みの構造体にはつけられないらしい。今後対応してもらえるとよさそうな機能。
#[derive(Debug, Clone)]
pub struct Price<C: Currency>(f32, PhantomData<C>);

#[nutype(validate(finite, min = -100.0, max = 100.0))]
#[derive(*)]
pub struct Percent(f32);

fn main() {}

#[test]
fn ticker_symbol() {
    let ticker_symbol = "GOOGL";
    let symbol = TickerSymbol::new(ticker_symbol);
    claim::assert_ok!(symbol);

    let ticker_symbol = "GOOG";
    let symbol = TickerSymbol::new(ticker_symbol);
    claim::assert_ok!(symbol);

    // change the string to upper case
    let ticker_symbol = "goog";
    let symbol = TickerSymbol::new(ticker_symbol);
    claim::assert_ok!(symbol);
}

#[test]
fn sanitize_ticker_symbol() {
    let ticker_symbol = "    GOOG    ";
    let symbol = TickerSymbol::new(ticker_symbol);
    claim::assert_ok!(symbol);

    let ticker_symbol = "goog";
    let symbol = TickerSymbol::new(ticker_symbol);
    claim::assert_ok!(symbol);
}

#[test]
fn passed_empty_string_to_symbol() {
    let empty_symbol = "".to_string();
    let symbol = TickerSymbol::new(empty_symbol);
    claim::assert_err!(symbol.clone());
    claim::assert_matches!(symbol, Err(TickerSymbolError::Empty));
}

#[test]
fn passed_invalid_symbol() {
    let not_exist_such_symbol = "NONEXIST".to_string();
    let symbol = TickerSymbol::new(not_exist_such_symbol);
    claim::assert_err!(symbol.clone());
    claim::assert_matches!(symbol, Err(TickerSymbolError::TooLong));
}

#[test]
fn correct_percent() {
    let num = 0.589;
    let percent = Percent::new(num);
    claim::assert_ok!(percent);

    let num = -1.187;
    let percent = Percent::new(num);
    claim::assert_ok!(percent);
}

#[test]
fn passed_invalid_percent() {
    let over_hundred = 101.0;
    let percent = Percent::new(over_hundred);
    claim::assert_err!(percent.clone());
    claim::assert_matches!(percent, Err(PercentError::TooBig));

    let nan = f32::NAN;
    let percent = Percent::new(nan);
    claim::assert_err!(percent.clone());
    claim::assert_matches!(percent, Err(PercentError::NotFinite));
}
