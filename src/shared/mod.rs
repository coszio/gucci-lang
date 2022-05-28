pub(crate) mod op_code;
pub mod quad;


pub(crate) const W: usize = 14;

pub(crate) type Span = std::ops::Range<usize>;

pub(crate) type Spanned<T> = (T, Span);