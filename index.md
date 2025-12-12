---
layout: default
title: Learn Ada for Z80
---

# Learn Ada for Z80/CP/M

**100 Ada programming examples** for the [uada80 compiler](https://github.com/avwohl/uada80) targeting the Z80 processor running CP/M.

---

## Quick Start

```ada
-- hello_world.adb
with Ada.Text_IO;
procedure Hello_World is
begin
   Ada.Text_IO.Put_Line("Hello, Z80!");
end Hello_World;
```

---

## Free Ada Learning Resources

### Official Resources
- [AdaCore Learn Platform](https://learn.adacore.com/) - Free interactive courses
- [Introduction to Ada (PDF)](https://learn.adacore.com/pdf_books/courses/intro-to-ada.pdf) - Comprehensive guide
- [Ada Resource Association](https://www.adaic.org/learn/) - Tutorials and books

### Free E-Books
- [Ada Programming Wikibook](https://en.wikibooks.org/wiki/Ada_Programming) ([PDF](https://upload.wikimedia.org/wikipedia/commons/8/8d/Ada_Programming.pdf))
- [Ada 95 Tutorial](https://perso.telecom-paristech.fr/pautet/Ada95/a95list.htm) - 33 chapters
- [Ada95 Lovelace Tutorial](https://dwheeler.com/lovelace/)
- [awesome-ada on GitHub](https://github.com/ohenley/awesome-ada)

---

## Example Categories

| Category | Examples | Topics |
|----------|----------|--------|
| [01 Basics](01_basics/) | 8 | Hello world, comments, program structure |
| [02 Types](02_types/) | 9 | Integers, booleans, enums, subtypes |
| [03 Variables](03_variables/) | 3 | Variables, constants, initialization |
| [04 Operators](04_operators/) | 5 | Arithmetic, relational, logical |
| [05 Control Flow](05_control_flow/) | 11 | If, case, loops, expressions |
| [06 Arrays](06_arrays/) | 7 | Arrays, slices, multidimensional |
| [07 Records](07_records/) | 5 | Records, variants, nesting |
| [08 Subprograms](08_subprograms/) | 10 | Procedures, functions, operators |
| [09 Packages](09_packages/) | 6 | Packages, private types |
| [10 Exceptions](10_exceptions/) | 5 | Exception handling |
| [11 Access Types](11_access_types/) | 3 | Pointers, linked lists |
| [12 Generics](12_generics/) | 4 | Generic programming |
| [13 Tasking](13_tasking/) | 4 | Concurrent programming |
| [14 Attributes](14_attributes/) | 3 | Type and object attributes |
| [15 Strings](15_strings/) | 1 | String handling |
| [16 I/O](16_io/) | 2 | Input/output |
| [17 Derived Types](17_derived_types/) | 1 | Type derivation |
| [18 Pragmas](18_pragmas/) | 1 | Compiler directives |
| [19 Contracts](19_contracts/) | 1 | Pre/post conditions |
| [20 Applications](20_applications/) | 11 | Calculator, sorting, games |

---

## Suggested Learning Path

1. **[Basics](01_basics/)** - Start here with hello world
2. **[Types](02_types/)** - Ada's strong type system
3. **[Variables](03_variables/)** - Data storage
4. **[Operators](04_operators/)** - Expressions
5. **[Control Flow](05_control_flow/)** - Logic and loops
6. **[Arrays](06_arrays/)** - Collections
7. **[Records](07_records/)** - Structures
8. **[Subprograms](08_subprograms/)** - Functions and procedures
9. **[Packages](09_packages/)** - Modularity
10. **[Exceptions](10_exceptions/)** - Error handling
11. **[Access Types](11_access_types/)** - Pointers
12. **[Generics](12_generics/)** - Templates
13. **[Tasking](13_tasking/)** - Concurrency
14. **[Applications](20_applications/)** - Put it all together!

---

## Z80/CP/M Considerations

These examples work within Z80/CP/M constraints:

| Constraint | Detail |
|------------|--------|
| Memory | ~57K TPA on 64K system |
| Integers | 16-bit (-32768 to 32767) |
| Floating Point | Software library (optional) |
| Tasking | Via timer interrupts |
| File I/O | 128-byte sectors, 8.3 filenames |

---

## Compilation

```bash
# Compile with uada80
python -m uada80 hello_world.adb -o hello.com

# Run in CP/M emulator
cpmemu hello.com
```

---

## Browse Source

All examples are in the [GitHub repository](https://github.com/avwohl/learn-ada-z80).

Each `.adb` file includes detailed comments explaining the concept.

---

*Part of the [uada80](https://github.com/avwohl/uada80) Ada compiler project for Z80/CP/M*
