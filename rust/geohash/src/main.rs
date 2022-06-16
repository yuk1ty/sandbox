fn encode_geohash(lat: f32, lon: f32, bits: i32) -> i64 {
    let mut min_lat = -90f32;
    let mut max_lat = 90f32;
    let mut min_lon = -180f32;
    let mut max_lon = 180f32;

    let mut result = 0;

    for i in 0..bits {
        if i % 2 == 0 {
            let midpoint = (min_lon + max_lon) / 2f32;
            if lon < midpoint {
                result <<= 1;
                max_lon = midpoint;
            } else {
                result = result << 1 | 1;
                min_lon = midpoint;
            }
        } else {
            let midpoint = (min_lat + max_lat) / 2f32;
            if lat < midpoint {
                result <<= 1;
                max_lat = midpoint;
            } else {
                result = result << 1 | 1;
                min_lat = midpoint;
            }
        }
    }

    result
}

fn main() {
    let geohash = encode_geohash(35.658034, 139.701636, 24);
    println!("{}", geohash);
}
