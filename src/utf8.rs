use std::str;

use parse;

// This exposes a ham-fisted UTF-8 decoder. We need this because we really want
// to match on `&[u8]`, not `&str`, since file names _and_ patterns needn't be
// valid UTF-8. Thus, we use this module to extract a `char` from a `&[u8]`.
//
// This is always used when parsing a glob pattern. During matching, this is
// only used with the "slow" engine. When using regexes to match, regex will
// handle things more intelligently.
//
// (We could use bstr, but the extra dependency bloat just isn't worth it.)

/// A representation of a sinlge character in a byte string.
///
/// When the next position in the byte string consists of a valid UTF-8 encoded
/// Unicode scalar value, then Char::Codepoint is always used. In all other
/// cases, the next byte is returned as Char::Byte.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Char {
    Codepoint(char),
    Byte(u8),
}

impl Char {
    /// Return the length, in bytes, used by this character.
    pub fn len(&self) -> usize {
        match *self {
            Char::Codepoint(ch) => ch.len_utf8(),
            Char::Byte(_) => 1,
        }
    }
}

impl PartialEq<char> for Char {
    fn eq(&self, other: &char) -> bool {
        match *self {
            Char::Codepoint(ch) => ch == *other,
            Char::Byte(byte) => byte.is_ascii() && byte as char == *other,
        }
    }
}

impl PartialEq<Char> for char {
    fn eq(&self, other: &Char) -> bool {
        *other == *self
    }
}

/// Return either the next UTF-8 codepoint in the slice, or the next byte in
/// the slice if the next byte does not correspond to a prefix of a valid UTF-8
/// encoded codepoint.
///
/// If the slice is empty, then None is returned.
pub fn decode(slice: &[u8]) -> Option<Char> {
    let len = utf8_len(slice)?;
    if len > slice.len() {
        return Some(Char::Byte(slice[0]));
    }
    Some(match str::from_utf8(&slice[..len]) {
        Ok(string) => Char::Codepoint(
            string.chars().next().expect("exactly one codepoint"),
        ),
        Err(_) => Char::Byte(slice[0]),
    })
}

/// Return the length of the next UTF-8 encoded codepoint in the slice. If the
/// slice is empty, then None is returned.
fn utf8_len(slice: &[u8]) -> Option<usize> {
    let b = *slice.get(0)?;
    let len = if b <= 0x7F {
        1
    } else if b <= 0b110_11111 {
        2
    } else if b <= 0b1110_1111 {
        3
    } else {
        4
    };
    Some(len)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn chr(ch: char) -> Char {
        Char::Codepoint(ch)
    }

    fn byte(b: u8) -> Char {
        Char::Byte(b)
    }

    #[test]
    fn utf8_empty() {
        assert!(decode(&[]).is_none());
    }

    #[test]
    fn utf8() {
        let dec = |slice| decode(slice).unwrap();

        // ASCII
        assert_eq!(dec(b"a"), chr('a'));
        assert_eq!(dec(b"ab"), chr('a'));
        assert_eq!(dec(b"a\xFF"), chr('a'));

        // 2-byte encoded codepoint
        assert_eq!(dec("β".as_bytes()), chr('β'));
        assert_eq!(dec("aβ".as_bytes()), chr('a'));
        assert_eq!(dec("ββ".as_bytes()), chr('β'));
        assert_eq!(dec("βa".as_bytes()), chr('β'));

        // 3-byte encoded codepoint
        assert_eq!(dec("☃".as_bytes()), chr('☃'));
        assert_eq!(dec("a☃".as_bytes()), chr('a'));
        assert_eq!(dec("☃☃".as_bytes()), chr('☃'));
        assert_eq!(dec("☃a".as_bytes()), chr('☃'));

        // 4-byte encoded codepoint
        assert_eq!(dec("𝛃".as_bytes()), chr('𝛃'));
        assert_eq!(dec("a𝛃".as_bytes()), chr('a'));
        assert_eq!(dec("𝛃𝛃".as_bytes()), chr('𝛃'));
        assert_eq!(dec("𝛃a".as_bytes()), chr('𝛃'));

        // no valid prefix
        assert_eq!(dec(b"\xFF"), byte(b'\xFF'));
        assert_eq!(dec(b"\xFFa"), byte(b'\xFF'));
        assert_eq!(dec(b"\xFF\xE2\x98\x83"), byte(b'\xFF'));
        assert_eq!(dec(b"\xFF\xFF"), byte(b'\xFF'));

        // valid 1-byte prefix
        assert_eq!(dec(b"\xCE"), byte(b'\xCE'));
        assert_eq!(dec(b"\xCEa"), byte(b'\xCE'));
        assert_eq!(dec(b"\xCE\xFF"), byte(b'\xCE'));

        // valid 2-byte prefix
        assert_eq!(dec(b"\xE2\x98"), byte(b'\xE2'));
        assert_eq!(dec(b"\xE2\x98a"), byte(b'\xE2'));
        assert_eq!(dec(b"\xE2\x98\xFF"), byte(b'\xE2'));
        assert_eq!(dec(b"\xE2"), byte(b'\xE2'));
        assert_eq!(dec(b"\xE2a"), byte(b'\xE2'));
        assert_eq!(dec(b"\xE2\xFF"), byte(b'\xE2'));
        assert_eq!(dec(b"\x98"), byte(b'\x98'));
        assert_eq!(dec(b"\x98a"), byte(b'\x98'));
        assert_eq!(dec(b"\x98\xFF"), byte(b'\x98'));

        // valid 3-byte prefix
        assert_eq!(dec(b"\xF0\x9D\x9B"), byte(b'\xF0'));
        assert_eq!(dec(b"\xF0\x9D\x9Ba"), byte(b'\xF0'));
        assert_eq!(dec(b"\xF0\x9D\x9B\xFF"), byte(b'\xF0'));
    }
}
