pub trait IterExt: Iterator + Sized {
    fn join(self, s: &str) -> String;
}

impl<'a, I, T> IterExt for I
where
    I: Iterator<Item = T>,
    T: std::ops::Deref<Target = str> + 'a,
{
    fn join(self, s: &str) -> String {
        self.fold(String::new(), |mut a, c| {
            if !a.is_empty() {
                a.push_str(s);
            }
            a.push_str(&c);
            a
        })
    }
}
