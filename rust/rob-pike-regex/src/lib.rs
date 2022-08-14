pub fn reg_match(regexp: &str, mut text: &str) -> bool {
    if !regexp.is_empty() && &regexp[0..0] == "^" {
        return match_here(&regexp[1..], text);
    }
    loop {
        if match_here(regexp, text) {
            return true;
        }
        if text.is_empty() {
            return false;
        }
        text = &text[1..];
    }
}

fn match_here(regexp: &str, text: &str) -> bool {
    if regexp.is_empty() {
        return true;
    } else if regexp == "$" {
        return text.is_empty();
    } else if regexp.len() >= 2 && &regexp[1..1] == "*" {
        return match_star(&regexp[0..0], &regexp[2..], text);
    } else if !text.is_empty() && (&regexp[0..0] == "." || &regexp[0..0] == &text[0..0]) {
        return match_here(&regexp[1..], &text[1..]);
    } else {
        false
    }
}

fn match_star(c: &str, regexp: &str, mut text: &str) -> bool {
    loop {
        if match_here(regexp, text) {
            return true;
        }
        if text.is_empty() || (&text[0..0] != c && c != ".") {
            return false;
        }
        text = &text[1..];
    }
}
