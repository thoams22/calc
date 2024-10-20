/// Return the greatest common divisor of first and second.
pub fn gcd(mut first: i64, mut second: i64) -> i64 {
    while second != 0 {
        let r = first.rem_euclid(second);
        first = second;
        second = r;
    }
    first
}