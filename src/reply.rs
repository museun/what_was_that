use std::borrow::Cow;

#[derive(Debug)]
pub enum Reply<'a> {
    Single(Cow<'a, str>),
    Many(Cow<'a, [Cow<'a, str>]>),
    #[allow(dead_code)]
    None,
}

impl<'b, 'a: 'b> Reply<'a> {
    pub fn iter(&'b self) -> impl Iterator<Item = &'b str> + 'b {
        match self {
            Self::Single(s) => std::slice::from_ref(s),
            Self::Many(n) => &**n,
            Self::None => &[],
        }
        .iter()
        .map(std::ops::Deref::deref)
    }
}
