use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq)]
pub struct Member {
    name: MemberName,
}

#[derive(Debug, PartialEq, Eq)]
pub struct MemberName(String);

impl TryFrom<String> for MemberName {
    type Error = MemberError;

    fn try_from(name: String) -> Result<Self, Self::Error> {
        if name.len() == 0 {
            Err(MemberError::NameShouldNotBeEmpty)
        } else if name.len() > 24 {
            Err(MemberError::NameShouldBeLessThanEqual24)
        } else {
            Ok(Self(name))
        }
    }
}

#[derive(Debug)]
pub enum MemberError {
    NameShouldNotBeEmpty,
    NameShouldBeLessThanEqual24,
}

pub struct Cache(HashMap<String, Member>);

#[cfg(test)]
mod tests {
    use claim::{assert_err, assert_matches, assert_none, assert_some, assert_some_eq};

    use super::*;

    #[test]
    fn raise_error_if_name_is_empty() {
        let name = "".to_string();
        let result = MemberName::try_from(name);
        // no need to write like below
        // assert!(result.is_err());
        assert_err!(result);
    }

    #[test]
    fn raise_specific_error_if_name_len_is_greater_than_equal_25() {
        let name = "a".repeat(25);
        let result = MemberName::try_from(name);
        assert_matches!(result, Err(MemberError::NameShouldBeLessThanEqual24));
    }

    #[test]
    fn cache_works() {
        let dummy = HashMap::from_iter(vec![
            (
                "id-1".to_string(),
                Member {
                    name: MemberName::try_from("Socrates".to_string()).unwrap(),
                },
            ),
            (
                "id-2".to_string(),
                Member {
                    name: MemberName::try_from("Plato".to_string()).unwrap(),
                },
            ),
            (
                "id-3".to_string(),
                Member {
                    name: MemberName::try_from("Aristotle".to_string()).unwrap(),
                },
            ),
        ]);
        let cache = Cache(dummy);
        assert_some!(cache.0.get("id-1"));
        assert_some_eq!(
            cache.0.get("id-2"),
            &Member {
                name: MemberName::try_from("Plato".to_string()).unwrap(),
            }
        );
        assert_none!(cache.0.get("id-4"));
    }
}
