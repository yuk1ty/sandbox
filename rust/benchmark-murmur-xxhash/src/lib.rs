use fasthash::{murmur3, xx};

pub fn hash_with_murmur3(s: &str) -> u32 {
    murmur3::hash32(s.as_bytes()) % 128
}

pub fn hash_with_xxhash(s: &str) -> u32 {
    xx::hash32(s.as_bytes()) % 128
}
