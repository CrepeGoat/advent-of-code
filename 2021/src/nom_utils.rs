use core::ops::RangeFrom;
use nom::combinator::map;
use nom::{
    bits::complete::take as take_bits, error::ParseError, IResult, InputIter, InputLength, Slice,
};

pub fn take_rem<I, E: ParseError<(I, usize)>>(
) -> impl Fn((I, usize)) -> IResult<(I, usize), (u8, usize), E>
where
    I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
{
    |(input, bit_offset): (I, usize)| {
        let bitlen = (8usize - bit_offset) % 8usize;
        map(take_bits(bitlen), move |bits| (bits, bitlen))((input, bit_offset))
    }
}
