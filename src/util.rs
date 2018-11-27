use std::iter::Peekable;

pub fn peekable_not_empty<T>(p: &mut Peekable<T>) -> bool
where
	T: Iterator,
{
	p.peek().is_some()
}
