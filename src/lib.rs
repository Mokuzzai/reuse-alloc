#![feature(allocator_api)]
#![feature(slice_ptr_get)]
#![feature(alloc_layout_extra)]
#![feature(new_uninit)]

use std::alloc::Global;
use std::alloc::Layout;
use std::alloc::Allocator;
use std::alloc;

use std::ptr::NonNull;
use std::ptr;

use std::mem::MaybeUninit;
use std::mem;

pub struct Alloc<A: Allocator = Global> {
	ptr: NonNull<u8>,
	layout: Layout,
	alloc: A,
}

impl<A: Allocator> Alloc<A> {
	/// # Safety
	///
	/// `ptr` must point to an allocation allocated with [`Global`] with the layout `layout`
	///
	/// the ownership of the allocation is transfered to this function
	///
	pub unsafe fn new(ptr: NonNull<u8>, layout: Layout) -> Alloc {
		Alloc::new_in(ptr, layout, Global)
	}
	/// # Safety
	///
	/// `ptr` must point to an allocation allocated with `alloc` with the layout `layout`
	///
	/// the ownership of the allocation is transfered to this function
	///
	pub unsafe fn new_in(ptr: NonNull<u8>, layout: Layout, alloc: A) -> Self {
		Self { ptr, layout, alloc }
	}
	pub fn alloc_in(layout: Layout, alloc: A) -> Self {
		if let 0 = layout.size() {
			unsafe { Self::new_in(NonNull::new_unchecked(layout.align() as *mut u8), layout, alloc) }
		} else {
			match alloc.allocate(layout) {
				Ok(ptr) => unsafe { Self::new_in(ptr.as_non_null_ptr(), layout, alloc) },
				Err(_) => alloc::handle_alloc_error(layout),
			}
		}
	}
	pub fn is_zst_no_alloc(&self) -> bool {
		self.layout.size() != 0
	}
	pub fn to_raw_parts_with_alloc(self) -> (NonNull<u8>, Layout, A) {
		use std::mem::ManuallyDrop;
		let this = ManuallyDrop::new(self);
		unsafe {(
			ptr::read(&this.ptr),
			ptr::read(&this.layout),
			ptr::read(&this.alloc),
		)}
	}
	pub fn try_to_raw_parts_with_alloc<T>(self, f: impl FnOnce(Layout) -> Result<T, TryFromAllocErrorKind>) -> Result<(NonNull<u8>, T, A), TryFromAllocError<A>> {
		match f(self.layout) {
			Ok(t) => {
				let (ptr, _, alloc) = self.to_raw_parts_with_alloc();
				Ok((ptr, t, alloc))
			},
			Err(kind) => Err(TryFromAllocError { alloc: self, kind }),
		}
	}
}

impl Alloc<Global> {
	pub fn to_raw_parts(self) -> (NonNull<u8>, Layout) {
		let (a, b, ..) = self.to_raw_parts_with_alloc();
		(a, b)
	}
}

impl<A: Allocator + Clone> Clone for Alloc<A> {
	fn clone(&self) -> Alloc<A> {
		if self.is_zst_no_alloc() {
			unsafe { Self::new_in(self.ptr, self.layout, self.alloc.clone()) }
		} else {
			Self::alloc_in(self.layout, self.alloc.clone())
		}
	}
}

impl<A: Allocator> Drop for Alloc<A> {
	fn drop(&mut self) {
		unsafe { self.alloc.deallocate(self.ptr, self.layout) }
	}
}

pub enum TryFromAllocErrorKind {
	Align {
		expected: usize,
		got: usize,
	},
	Length {
		overflow: isize,
	}
}

pub struct TryFromAllocError<A: Allocator> {
	kind: TryFromAllocErrorKind,
	alloc: Alloc<A>,
}

type TryFromAllocResult<T, A> = Result<T, TryFromAllocError<A>>;

pub trait TryFromAlloc: Sized {
	type Alloc: Allocator;

	fn try_from_alloc(_: Alloc<Self::Alloc>) -> TryFromAllocResult<Self, Self::Alloc>;
}

pub fn reuse<T, U, A: Allocator>(t: T) -> TryFromAllocResult<U, A>
where
	T: Into<Alloc<A>>,
	U: TryFromAlloc<Alloc = A>,
{
	let alloc = t.into();

	U::try_from_alloc(alloc)
}

pub fn reuse_box<T, U, A: Allocator>(t: Box<T, A>, u: U) -> Result<Box<U, A>, (TryFromAllocError<A>, U)>
where
	Box<T, A>: Into<Alloc<A>>,
	U: TryFromAlloc<Alloc = A>,
{
	let alloc = t.into();

	match Box::<MaybeUninit<U>, A>::try_from_alloc(alloc) {
		Ok(x) => Ok(Box::write(x, u)),
		Err(e) => Err((e, u)),
	}
}

pub fn reuse_box_with<T, U, A: Allocator>(t: Box<T, A>, u: impl FnOnce() -> U) -> Result<Box<U, A>, TryFromAllocError<A>>
where
	Box<T, A>: Into<Alloc<A>>,
	U: TryFromAlloc<Alloc = A>,
{
	let alloc = t.into();

	match Box::<MaybeUninit<U>, A>::try_from_alloc(alloc) {
		Ok(x) => Ok(Box::write(x, u())),
		Err(e) => Err(e),
	}
}

impl<T, A: Allocator> From<Vec<T, A>> for Alloc<A> {
	fn from(mut x: Vec<T, A>) -> Self {
		x.clear();

		let (ptr, .., cap, alloc) = x.into_raw_parts_with_alloc();
		let ptr = unsafe { NonNull::new_unchecked(ptr) };
		let ptr = ptr.cast();

		let layout = unsafe { Layout::array::<T>(cap).unwrap_unchecked() };

		unsafe { Alloc::new_in(ptr, layout, alloc) }
	}
}

impl<'a, T, A: Allocator + Clone> From<&'a mut Vec<T, A>> for Alloc<A> {
	fn from(x: &'a mut Vec<T, A>) -> Self {
		mem::replace(x, Vec::new_in(x.allocator().clone())).into()
	}
}

impl<T, A: Allocator> From<Box<T, A>> for Alloc<A> {
	fn from(mut x: Box<T, A>) -> Self {
		let (ptr, alloc) = Box::into_raw_with_allocator(x);

		unsafe { ptr::drop_in_place(ptr) }

		let ptr = unsafe { NonNull::new_unchecked(ptr) };
		let ptr = ptr.cast();

		let layout = Layout::new::<T>();

		unsafe { Alloc::new_in(ptr, layout, alloc) }
	}
}

impl<T, A: Allocator> TryFromAlloc for Vec<T, A> {
	type Alloc = A;

	fn try_from_alloc(alloc: Alloc<A>) -> TryFromAllocResult<Self, Self::Alloc> {
		let target = Layout::new::<T>();

		let (ptr, cap, alloc) = alloc.try_to_raw_parts_with_alloc(|layout| {
			if layout.align() == target.align() {
				Err(TryFromAllocErrorKind::Align { expected: target.align(), got: layout.align() })
			} else {
				let overflow = layout.size() % target.size();

				if overflow == 0 {
					Ok(layout.size() / target.size())
				} else {
					Err(TryFromAllocErrorKind::Length { overflow: overflow as isize })
				}
			}
		})?;

		unsafe { Ok(Self::from_raw_parts_in(ptr.as_ptr().cast(), 0, cap, alloc)) }
	}
}


impl<T, A: Allocator> TryFromAlloc for Box<MaybeUninit<T>, A> {
	type Alloc = A;

	fn try_from_alloc(alloc: Alloc<A>) -> TryFromAllocResult<Self, Self::Alloc> {
		let target = Layout::new::<T>();

		let (ptr, _, alloc) = alloc.try_to_raw_parts_with_alloc(|layout| {
			if layout.align() != target.align() {
				Err(TryFromAllocErrorKind::Align { expected: target.align(), got: layout.align() })
			} else {
				if layout.size() != target.size() {
					Err(TryFromAllocErrorKind::Length { overflow: target.size() as isize - layout.size() as isize })
				} else {
					Ok(())
				}
			}
		})?;

		unsafe { Ok(Box::from_raw_in(ptr.as_ptr().cast(), alloc)) }
	}
}













