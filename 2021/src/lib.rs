#[macro_use]
pub mod utils;
pub mod _2d_int;
pub mod grid;
pub mod nom_utils;
pub mod vectorized;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
