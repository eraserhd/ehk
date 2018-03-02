
fn is_prime(p : u32) -> bool {
    if p == 2 { return true; }
    if p % 2 == 0 { return false; }
    !(0..p).map(|x| 2*x+3).take_while(|x| x*x <= p).any(|x| p % x == 0)
}

pub fn nth(n : usize) -> Result<u32,&'static str> {
    if n <= 0 {
        return Err("Invalid prime index");
    }
    match (2..).filter(|&x| is_prime(x)).nth(n-1)  {
        Some(p) => Ok(p),
        None    => Err("Shouldn't get here")
    }
}
