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

// pub trait FutureExt
// where
//     Self: std::future::Future + Sized + Send + Sync,
//     Self::Output: Send + Sync,
// {
//     fn race<F>(self, other: F) -> Or<Self, F>
//     where
//         F: std::future::Future + Send + Sync,
//         F::Output: Send + Sync;
// }

// impl<T> FutureExt for T
// where
//     T: std::future::Future + Sized + Send + Sync,
//     T::Output: Send + Sync,
// {
//     fn race<F>(self, other: F) -> Or<Self, F>
//     where
//         F: std::future::Future + Send + Sync,
//         F::Output: Send + Sync,
//     {
//         let left = self;
//         Or {
//             left,
//             right: other,
//             biased: false,
//         }
//     }
// }

// pin_project_lite::pin_project! {
//     pub struct Or<L, R> {
//         #[pin] left: L,
//         #[pin] right: R,
//         biased: bool
//     }
// }

// impl<L, R> std::future::Future for Or<L, R>
// where
//     L: std::future::Future + Send + Sync,
//     R: std::future::Future + Send + Sync,

//     L::Output: Send + Sync,
//     R::Output: Send + Sync,
// {
//     type Output = Either<L::Output, R::Output>;

//     fn poll(
//         self: std::pin::Pin<&mut Self>,
//         cx: &mut std::task::Context<'_>,
//     ) -> std::task::Poll<Self::Output> {
//         use {
//             std::task::Poll,
//             Either::{Left, Right},
//         };

//         let this = self.project();

//         macro_rules! poll {
//             ($expr:ident => $map:expr) => {
//                 if let Poll::Ready(t) = this.$expr.poll(cx).map($map) {
//                     return Poll::Ready(t);
//                 }
//             };
//         }

//         if *this.biased {
//             poll!(left => Left);
//             poll!(right => Right);
//             return Poll::Pending;
//         }

//         if fastrand::bool() {
//             poll!(left => Left);
//             poll!(right => Right);
//         } else {
//             poll!(right => Right);
//             poll!(left => Left);
//         }

//         Poll::Pending
//     }
// }

// pub enum Either<L, R> {
//     Left(L),
//     Right(R),
// }
