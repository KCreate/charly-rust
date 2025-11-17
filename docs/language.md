# Charly Programming Language

## Project Goals

- Statically typed language
- Type inference
- Closures
- Concurrency using fibers
- No exceptions, errors must be declared and handled explicitly
- Type system
	- Declare types with fixed layout
	- No inheritance
	- Interfaces which can inherit
	- Impl declarations
		- Implement methods for a type
		- Implement interfaces for a type
	- Primitive types
		- i8, i16, i32, i64
		- u8, u16, u32, u64
		- f32, f64
		- strings (enforced utf8 encoding)
		- characters (utf8 codepoint)
		- bytes
		- boolean
		- null
		- tuples
		- lists
		- dicts
		- types
		- functions
- Module system
	- Project -> Scope -> Module -> Type

## Implementation
