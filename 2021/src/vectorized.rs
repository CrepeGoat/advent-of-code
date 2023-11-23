use std::cmp::PartialEq;
use std::convert::From;
use std::ops::Add;
use std::str::FromStr;

#[derive(Debug, PartialEq)]
pub struct Vectorized<T, const N: usize>(pub [T; N]);

impl<const N: usize> FromStr for Vectorized<bool, N> {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() != N {
            return Err("invalid input length");
        }

        let mut array = [false; N];
        for (a, bit) in array.iter_mut().zip(s.chars()) {
            *a = match bit {
                '1' => true,
                '0' => false,
                _ => return Err("invalid character"),
            }
        }

        Ok(Self(array))
    }
}

impl<T: Clone, const N: usize> Clone for Vectorized<T, N> {
    fn clone(&self) -> Self {
        let data = unsafe {
            /*
            // Create an uninitialized array of `MaybeUninit`. The `assume_init` is
            // safe because the type we are claiming to have initialized here is a
            // bunch of `MaybeUninit`s, which do not require initialization.
            let mut data: [MaybeUninit<T>; N] = unsafe { MaybeUninit::uninit().assume_init() };

            // Dropping a `MaybeUninit` does nothing. Thus using raw pointer
            // assignment instead of `ptr::write` does not cause the old
            // uninitialized value to be dropped. Also if there is a panic during
            // this loop, we have a memory leak, but there is no memory safety
            // issue.
            for (elem, vself) in (&mut data[..]).iter_mut().zip(&self.0[..]) {
                elem.write(vself.clone());
            }

            // Everything is initialized. Transmute the array to the
            // initialized type.
            unsafe { transmute::<_, [T; N]>(data) }
            */
            let mut data: [T; N] = std::mem::uninitialized();
            for (elem, vself) in (&mut data[..]).iter_mut().zip(&self.0[..]) {
                std::ptr::write(elem as *mut T, vself.clone());
            }
            data
        };

        Vectorized(data)
    }
}

impl<T, const N: usize> Vectorized<T, N> {
    pub fn map<U, F: Fn(&T) -> U>(&self, func: F) -> Vectorized<U, N> {
        // copied from rust reference:
        // https://doc.rust-lang.org/std/mem/union.MaybeUninit.html#initializing-an-array-element-by-element
        let data = unsafe {
            /*
            // Create an uninitialized array of `MaybeUninit`. The `assume_init` is
            // safe because the type we are claiming to have initialized here is a
            // bunch of `MaybeUninit`s, which do not require initialization.
            let mut data: [MaybeUninit<U>; N] = unsafe { MaybeUninit::uninit().assume_init() };

            // Dropping a `MaybeUninit` does nothing. Thus using raw pointer
            // assignment instead of `ptr::write` does not cause the old
            // uninitialized value to be dropped. Also if there is a panic during
            // this loop, we have a memory leak, but there is no memory safety
            // issue.
            for (elem, vself) in (&mut data[..]).iter_mut().zip(&self.0[..]) {
                elem.write(func(vself));
            }

            // Everything is initialized. Transmute the array to the
            // initialized type.
            unsafe { std::mem::transmute::<_, [U; N]>(data) }
            */
            let mut data: [U; N] = std::mem::uninitialized();
            for (elem, vself) in (&mut data[..]).iter_mut().zip(&self.0[..]) {
                std::ptr::write(elem as *mut U, func(vself));
            }
            data
        };

        Vectorized(data)
    }

    pub fn combine<T2, U, F: Fn(&T, &T2) -> U>(
        &self,
        vec2: &Vectorized<T2, N>,
        func: F,
    ) -> Vectorized<U, N> {
        // copied from rust reference:
        // https://doc.rust-lang.org/std/mem/union.MaybeUninit.html#initializing-an-array-element-by-element
        let data = unsafe {
            /*
            // Create an uninitialized array of `MaybeUninit`. The `assume_init` is
            // safe because the type we are claiming to have initialized here is a
            // bunch of `MaybeUninit`s, which do not require initialization.
            let mut data: [MaybeUninit<U>; N] = unsafe { MaybeUninit::uninit().assume_init() };

            // Dropping a `MaybeUninit` does nothing. Thus using raw pointer
            // assignment instead of `ptr::write` does not cause the old
            // uninitialized value to be dropped. Also if there is a panic during
            // this loop, we have a memory leak, but there is no memory safety
            // issue.
            for ((elem, vself), v2) in (&mut data[..]).iter_mut().zip(&self.0[..]).zip(&vec2.0[..])
            {
                elem.write(func(vself, v2));
            }

            // Everything is initialized. Transmute the array to the
            // initialized type.
            unsafe { std::mem::transmute::<_, [U; N]>(data) }
            */

            let mut data: [U; N] = std::mem::uninitialized();
            for ((elem, vself), v2) in (&mut data[..]).iter_mut().zip(&self.0[..]).zip(&vec2.0[..])
            {
                std::ptr::write(elem as *mut U, func(vself, v2));
            }
            data
        };

        Vectorized(data)
    }
}

impl<T, U, const N: usize> Add for Vectorized<T, N>
where
    T: Add<Output = U> + Copy,
    U: Default + Copy,
{
    type Output = Vectorized<U, N>;
    fn add(self, rhs: Self) -> Self::Output {
        self.combine(&rhs, |&x, &y| x + y)
    }
}

impl<const N: usize> From<Vectorized<bool, N>> for u32 {
    fn from(vec: Vectorized<bool, N>) -> u32 {
        assert!(N <= 32, "cannot fit all {:?} bits in u32", N);

        let mut result: u32 = 0;
        for i in 0..N {
            if vec.0[i] {
                result |= 1_u32 << (N - 1 - i);
            }
        }

        result
    }
}
