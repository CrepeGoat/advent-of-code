use aoc_lib::nom_utils::take_rem;
use core::convert::TryFrom;
use core::fmt::Debug;
use core::str::FromStr;
use nom::bits::complete::take as take_bits;
use nom::combinator::map;
use std::io::BufRead;

use nom::sequence::tuple;
use std::convert::TryInto;

#[derive(Debug)]
pub enum ParseInputError<T: FromStr>
where
    <T as FromStr>::Err: Debug,
{
    InnerType(<T as FromStr>::Err),
    IO(std::io::Error),
}

fn parse_input<R: BufRead, T: FromStr>(mut reader: R) -> Result<T, ParseInputError<T>>
where
    <T as FromStr>::Err: Debug,
{
    let mut buffer = String::new();
    reader.read_line(&mut buffer).map_err(ParseInputError::IO)?;

    T::from_str(&buffer).map_err(ParseInputError::InnerType)
}

fn iter_pairs<I: Iterator>(
    iter: I,
) -> impl Iterator<Item = (<I as Iterator>::Item, <I as Iterator>::Item)> {
    let mut item_buffer = None;

    iter.filter_map(move |item| match item_buffer.take() {
        Some(item1) => Some((item1, item)),
        None => {
            item_buffer = Some(item);
            None
        }
    })
}

#[derive(Debug, PartialEq, Eq)]
struct HexBits(Vec<u8>);

impl FromStr for HexBits {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        iter_pairs(s.chars().map(|c| c.to_digit(16).ok_or("invalid digit")))
            .map(|(r1, r2)| -> Result<_, _> { Ok(((r1? << 4) | r2?).try_into().unwrap()) })
            .collect::<Result<Vec<_>, _>>()
            .map(HexBits)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LengthType {
    TotalLength(u16),
    SubPackets(u16),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OperatorType {
    Sum,
    Product,
    Minimum,
    Maximum,
    GreaterThan,
    LessThan,
    EqualTo,
}

impl TryFrom<u8> for OperatorType {
    type Error = u8;

    fn try_from(val: u8) -> Result<Self, Self::Error> {
        Ok(match val {
            0 => Self::Sum,
            1 => Self::Product,
            2 => Self::Minimum,
            3 => Self::Maximum,
            5 => Self::GreaterThan,
            6 => Self::LessThan,
            7 => Self::EqualTo,
            _ => return Err(val),
        })
    }
}

impl OperatorType {
    fn compute(&self, input_buffer: &[u64]) -> u64 {
        match self {
            Self::Sum => input_buffer.iter().sum::<u64>(),
            Self::Product => input_buffer.iter().product::<u64>(),
            Self::Minimum => *input_buffer.iter().min().unwrap(),
            Self::Maximum => *input_buffer.iter().max().unwrap(),
            Self::GreaterThan => {
                assert_eq!(input_buffer.len(), 2);
                (input_buffer[0] > input_buffer[1]) as u64
            }
            Self::LessThan => {
                assert_eq!(input_buffer.len(), 2);
                (input_buffer[0] < input_buffer[1]) as u64
            }
            Self::EqualTo => {
                assert_eq!(input_buffer.len(), 2);
                (input_buffer[0] == input_buffer[1]) as u64
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PacketType {
    Literal(u64),
    Operator(OperatorType, LengthType),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Packet(u8, PacketType);

impl Packet {
    fn read((stream, bit_offset): (&[u8], usize)) -> nom::IResult<(&[u8], usize), Self> {
        use LengthType::*;
        use PacketType::*;

        if let ((&[], 0), (0, _)) = take_rem()((stream, bit_offset))? {
            return Err(nom::Err::Failure(nom::error::Error {
                input: (stream, bit_offset),
                code: nom::error::ErrorKind::Eof,
            }));
        }

        let ((stream, bit_offset), (version_number, packet_id)): (_, (u8, u8)) =
            tuple((take_bits(3_usize), take_bits(3_usize)))((stream, bit_offset))?;

        let ((stream, bit_offset), parsed) = if packet_id == 4 {
            let mut result: u64 = 0;

            let (mut stream, mut bit_offset) = (stream, bit_offset);
            loop {
                let ((s, bo), (bit_continue, bits)): (_, (u64, u64)) =
                    tuple((take_bits(1_usize), take_bits(4_usize)))((stream, bit_offset))?;

                result = (result << 4) | bits;
                stream = s;
                bit_offset = bo;
                match bit_continue {
                    0 => break,
                    1 => (),
                    _ => unreachable!(),
                }
            }
            ((stream, bit_offset), Literal(result))
        } else {
            let ((stream, bit_offset), len_type_id): (_, u8) =
                take_bits(1_usize)((stream, bit_offset))?;
            let op = OperatorType::try_from(packet_id).unwrap();
            match len_type_id {
                0 => map(&take_bits(15_usize), |len| Operator(op, TotalLength(len)))((
                    stream, bit_offset,
                ))?,
                1 => map(&take_bits(11_usize), |len| Operator(op, SubPackets(len)))((
                    stream, bit_offset,
                ))?,
                _ => unreachable!(),
            }
        };

        Ok(((stream, bit_offset), Self(version_number, parsed)))
    }
}

pub fn stream_diff<'a>(first: (&'a [u8], usize), second: (&'a [u8], usize)) -> usize {
    let bytes: usize = unsafe { second.0.as_ptr().offset_from(first.0.as_ptr()) }
        .try_into()
        .unwrap();

    (8 * bytes + second.1) - first.1
}

fn iter_packets(hbits: &HexBits) -> impl '_ + Iterator<Item = (Packet, usize)> {
    itertools::unfold((&hbits.0[..], 0_usize), |input| -> Option<_> {
        let (new_input, packet) = Packet::read(*input).ok()?;
        let bitlen = stream_diff(*input, new_input);
        *input = new_input;

        Some((packet, bitlen))
    })
}

fn calc_expr(hbits: &HexBits) -> Result<u64, &'static str> {
    let packets = iter_packets(hbits).collect::<Vec<_>>();

    let mut packet_buffer = Vec::new();
    let mut bitlen_buffer = Vec::new();
    for (packet, bitlen) in packets.into_iter().rev() {
        let (packet, bitlen) = match packet.1 {
            PacketType::Literal(value) => (value, bitlen),
            PacketType::Operator(op_type, len_type) => {
                let (input_len, subpackets_bitlen) = match len_type {
                    LengthType::TotalLength(mut len) => {
                        let mut input_len = 0;
                        let subpacket_len = len;
                        while len != 0 {
                            len = len
                                .checked_sub(
                                    bitlen_buffer
                                        .pop()
                                        .ok_or("subpacket bit length is too long")?,
                                )
                                .ok_or("misaligned subpacket bit width")?;
                            input_len += 1;
                        }
                        (input_len, subpacket_len)
                    }
                    LengthType::SubPackets(input_len) => {
                        let subpacket_len = bitlen_buffer
                            .split_off(bitlen_buffer.len() - (input_len as usize))
                            .into_iter()
                            .sum();
                        (input_len, subpacket_len)
                    }
                };

                let inputs = packet_buffer
                    .split_off(packet_buffer.len() - (input_len as usize))
                    .into_iter()
                    .rev()
                    .collect::<Vec<_>>();
                let value = op_type.compute(&inputs[..]);

                (value, bitlen + (subpackets_bitlen as usize))
            }
        };
        packet_buffer.push(packet);
        bitlen_buffer.push(bitlen as u16);
    }

    if packet_buffer.len() != 1 {
        return Err("invalid equation structure");
    }
    if let Some(value) = packet_buffer.pop() {
        Ok(value)
    } else {
        unreachable!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest(input, expt_result,
        case(vec![1, 2, 3, 4], vec![(1, 2), (3, 4)])
    )]
    fn test_iter_pairs(input: Vec<i32>, expt_result: Vec<(i32, i32)>) {
        assert_eq!(
            iter_pairs(input.into_iter()).collect::<Vec<_>>(),
            expt_result
        );
    }

    #[rstest(input, expt_result,
        case("D2FE28", Ok(HexBits(vec![0xD2, 0xFE, 0x28]))),
        case("38006F45291200", Ok(HexBits(vec![0x38, 0x00, 0x6F, 0x45, 0x29, 0x12, 0x00]))),
        case("EE00D40C823060", Ok(HexBits(vec![0xEE, 0x00, 0xD4, 0x0C, 0x82, 0x30, 0x60]))),
        case("8A004A801A8002F478", Ok(HexBits(vec![0x8A, 0x00, 0x4A, 0x80, 0x1A, 0x80, 0x02, 0xF4, 0x78]))),
        case("620080001611562C8802118E34", Ok(HexBits(vec![0x62, 0x00, 0x80, 0x00, 0x16, 0x11, 0x56, 0x2C, 0x88, 0x02, 0x11, 0x8E, 0x34]))),
        case("C0015000016115A2E0802F182340", Ok(HexBits(vec![0xC0, 0x01, 0x50, 0x00, 0x01, 0x61, 0x15, 0xA2, 0xE0, 0x80, 0x2F, 0x18, 0x23, 0x40]))),
        case("A0016C880162017C3686B18A3D4780", Ok(HexBits(vec![0xA0, 0x01, 0x6C, 0x88, 0x01, 0x62, 0x01, 0x7C, 0x36, 0x86, 0xB1, 0x8A, 0x3D, 0x47, 0x80]))),

        case("D2F", Ok(HexBits(vec![0xD2]))),
        case("DG", Err("invalid digit")),
    )]
    fn test_hex_decoder_egs(input: &'static str, expt_result: Result<HexBits, &'static str>) {
        assert_eq!(HexBits::from_str(input), expt_result);
    }

    #[rstest(
        input,
        expt_result,
        case("D2FE28", 6),
        case("38006F45291200", 9),
        case("EE00D40C823060", 14),
        case("8A004A801A8002F478", 16),
        case("620080001611562C8802118E34", 12),
        case("C0015000016115A2E0802F182340", 23),
        case("A0016C880162017C3686B18A3D4780", 31)
    )]
    fn test_packet_decoder_version_sum_egs(input: &'static str, expt_result: usize) {
        let bytes = HexBits::from_str(input).unwrap();

        let calc_result: usize = iter_packets(&bytes).map(|(p, _)| p.0 as usize).sum();
        assert_eq!(calc_result, expt_result);
    }

    #[rstest(
        input,
        expt_result,
        case("D2FE28", vec![Packet(6, PacketType::Literal(2021))]),
        case("38006F45291200", vec![
            Packet(1, PacketType::Operator(OperatorType::LessThan, LengthType::TotalLength(27))),
            Packet(6, PacketType::Literal(10)),
            Packet(2, PacketType::Literal(20)),
        ]),
        case("EE00D40C823060", vec![
            Packet(7, PacketType::Operator(OperatorType::Maximum, LengthType::SubPackets(3))),
            Packet(2, PacketType::Literal(1)),
            Packet(4, PacketType::Literal(2)),
            Packet(1, PacketType::Literal(3)),
        ]),
    )]
    fn test_packet_decoder_egs(input: &'static str, expt_result: Vec<Packet>) {
        let bytes = HexBits::from_str(input).unwrap();
        for x in &bytes.0 {
            println!("{:#010b}", x);
        }

        let calc_result = iter_packets(&bytes).map(|(p, _)| p).collect::<Vec<_>>();
        assert_eq!(calc_result, expt_result);
    }

    #[rstest(
        input,
        expt_result,
        case("C200B40A82", 3),
        case("04005AC33890", 54),
        case("880086C3E88112", 7),
        case("CE00C43D881120", 9),
        case("D8005AC2A8F0", 1),
        case("F600BC2D8F", 0),
        case("9C005AC2F8F0", 0),
        case("9C0141080250320F1802104A08", 1)
    )]
    fn test_packet_calc_egs(input: &'static str, expt_result: u64) {
        let bytes = HexBits::from_str(input).unwrap();
        for x in &bytes.0 {
            println!("{:#010b}", x);
        }

        let calc_result = calc_expr(&bytes).unwrap();
        assert_eq!(calc_result, expt_result);
    }
}

fn main() {
    println!("Enter input:");
    let stdin = std::io::stdin();

    let bytes = parse_input::<_, HexBits>(stdin.lock()).unwrap();

    //let version_sum: usize = iter_packets(&bytes).map(|(p, _)| p.0 as usize).sum();
    //println!("version sum = {:?}", version_sum);

    let expr_value = calc_expr(&bytes).unwrap();
    println!("expression result = {:?}", expr_value);
}
