// extra leb128 function(s? later) the leb128 crate doesn't have

// https://en.wikipedia.org/wiki/LEB128#Unsigned_LEB128
pub fn write_unsigned_fixed<W: ?Sized + std::io::Write>(
    buffer: &mut W,
    len: usize,
    value: u64,
) -> std::io::Result<usize> {
    let mut value = value;
    let mut n: usize = 0;
    for _ in 0..len - 1 {
        let byte = (value as u8) | 0b1000_0000;
        value >>= 7;
        n += buffer.write(&[byte])?;
    }
    n += buffer.write(&[value as u8])?;
    Ok(n)
}

#[cfg(test)]
mod tests {
    use super::*;
    use leb128;

    #[test]
    fn test_write() {
        let mut buf = [0u8; 5];
        {
            let mut writable = &mut buf[..];
            write_unsigned_fixed(&mut writable, 5, 0).expect("write 0 failed");
        }
        let mut readable = &buf[..];
        assert_eq!(
            0,
            leb128::read::unsigned(&mut readable).expect("read 0 failed")
        );

        {
            let mut writable = &mut buf[..];
            write_unsigned_fixed(&mut writable, 5, 10000000).expect("write 10M failed");
        }
        let mut readable = &buf[..];
        assert_eq!(
            10000000,
            leb128::read::unsigned(&mut readable).expect("read 10M failed")
        );
    }

    #[test]
    fn test_write_u64_max() {
        // ceil(64 / 7) = 10
        let mut buf = [0u8; 10];
        {
            let mut writable = &mut buf[..];
            write_unsigned_fixed(&mut writable, 10, u64::MAX).expect("write failed");
        }
        let mut readable = &buf[..];
        assert_eq!(
            u64::MAX,
            leb128::read::unsigned(&mut readable).expect("read failed")
        );
    }
}
