use std::io::{self, BufReader, Read};

macro_rules! test {
    ($e:expr) => {
        match $e {
            Ok(v) => v,
            Err(err) => return Some(Err(err)),
        }
    };
}

pub struct Utf8Reader<T: Read> {
    reader: BufReader<T>,
}

// Idea taken from https://github.com/timothee-haudebourg/utf8-decode
impl<T: Read> Utf8Reader<T> {
    pub fn new(reader: T) -> Self {
        Self {
            reader: BufReader::new(reader),
        }
    }

    fn read_next_byte(&mut self) -> Option<Result<u8, io::Error>> {
        let mut buf = [0u8; 1];
        let n = test!(self.reader.read(&mut buf));

        if n != 1 {
            return None;
        }

        Some(Ok(buf[0]))
    }

    fn next_byte(&mut self) -> Option<Result<u32, io::Error>> {
        let c = test!(self.read_next_byte()?);

        if c & 0xC0 == 0x80 {
            Some(Ok((c & 0x3F) as u32))
        } else {
            Some(Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "invalid UTF-8 sequence.",
            )))
        }
    }

    fn decode_next(&mut self) -> Option<Result<u32, io::Error>> {
        let a = test!(self.read_next_byte()?) as u32;

        if a & 0x80 == 0x00 {
            Some(Ok(a))
        } else if a & 0xE0 == 0xC0 {
            let b = test!(self.next_byte()?);

            Some(Ok((a & 0x1F) << 6 | b))
        } else if a & 0xF0 == 0xE0 {
            let b = test!(self.next_byte()?);
            let c = test!(self.next_byte()?);

            Some(Ok((a & 0x0F) << 12 | b << 6 | c))
        } else if a & 0xF8 == 0xF0 {
            let b = test!(self.next_byte()?);
            let c = test!(self.next_byte()?);
            let d = test!(self.next_byte()?);

            Some(Ok((a & 0x07) << 18 | b << 12 | c << 6 | d))
        } else {
            Some(Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "invalid UTF-8 sequence.",
            )))
        }
    }
}

impl<T: Read> Iterator for Utf8Reader<T> {
    type Item = Result<char, io::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let c = test!(self.decode_next()?);

        Some(match char::try_from(c) {
            Ok(c) => Ok(c),
            Err(_) => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "invalid UTF-8 sequence",
            )),
        })
    }
}
