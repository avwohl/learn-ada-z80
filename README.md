# Ada Programming Examples for uada80 (Z80/CP/M)

This collection contains Ada example programs designed to teach Ada programming
concepts for the uada80 compiler targeting the Z80 processor running CP/M.

## Free Ada Learning Resources

### Official Resources

- **[AdaCore Learn Platform](https://learn.adacore.com/)** - Free interactive courses covering Ada fundamentals, tasking, SPARK, and more
- **[Introduction to Ada (PDF)](https://learn.adacore.com/pdf_books/courses/intro-to-ada.pdf)** - Comprehensive PDF guide from AdaCore
- **[Ada Resource Association](https://www.adaic.org/learn/)** - Tutorials, courses, books, and learning materials

### Free E-Books and Tutorials

- **[Ada Programming Wikibook](https://en.wikibooks.org/wiki/Ada_Programming)** - Community-maintained tutorial covering all Ada topics ([PDF version](https://upload.wikimedia.org/wikipedia/commons/8/8d/Ada_Programming.pdf))
- **[Ada Distilled](https://www.adaic.org/resources/add_content/docs/distilled/ada-distilled-24-january-2011-ada-2005-version.pdf)** - Concise guide for experienced programmers
- **[Ada 95 Tutorial (Coronado)](https://perso.telecom-paristech.fr/pautet/Ada95/a95list.htm)** - 33 chapters covering the entire language
- **[Ada95 Lovelace Tutorial](https://dwheeler.com/lovelace/)** - Self-paced tutorial by David Wheeler
- **[Free Ada E-Books at AdaForge](https://www.adaforge.org/Learn/)** - 18 free e-books including "Ada - a crash course"
- **[FreeComputerBooks Ada Section](https://www.freecomputerbooks.com/langAdaBooks.html)** - "Ada 95: The Craft of Object-Oriented Programming" and more

### Online Courses

- **[Class Central Ada Courses](https://www.classcentral.com/report/best-ada-courses/)** - Curated list of free and paid courses
- **[edX Ada Programming](https://www.edx.org/learn/ada-programming)** - Online courses from various providers
- **[Udemy Ada for Beginners](https://www.udemy.com/course/ada-programming-for-beginners/)** - Beginner-focused course

### Reference

- **[Ada Reference Manual](http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-TOC.html)** - Official language standard
- **[awesome-ada on GitHub](https://github.com/ohenley/awesome-ada)** - Curated list of Ada resources

---

## Example Programs

This collection contains **90+ example programs** organized by concept:

### [01_basics/](https://github.com/avwohl/learn-ada-z80/tree/main/01_basics) - Getting Started
- [`hello_world.adb`](https://github.com/avwohl/learn-ada-z80/blob/main/01_basics/hello_world.adb) - The simplest Ada program
- [`hello_use.adb`](https://github.com/avwohl/learn-ada-z80/blob/main/01_basics/hello_use.adb) - Using "use" clauses
- [`comments.adb`](https://github.com/avwohl/learn-ada-z80/blob/main/01_basics/comments.adb) - Ada comment syntax
- [`identifiers.adb`](https://github.com/avwohl/learn-ada-z80/blob/main/01_basics/identifiers.adb) - Naming rules
- [`reserved_words.adb`](https://github.com/avwohl/learn-ada-z80/blob/main/01_basics/reserved_words.adb) - Ada keywords
- [`program_structure.adb`](https://github.com/avwohl/learn-ada-z80/blob/main/01_basics/program_structure.adb) - Program organization
- [`semicolons.adb`](https://github.com/avwohl/learn-ada-z80/blob/main/01_basics/semicolons.adb) - Statement terminators
- [`blocks.adb`](https://github.com/avwohl/learn-ada-z80/blob/main/01_basics/blocks.adb) - Declare blocks

### 02_types/ - Type System
- `integer_types.adb` - Integer types and literals
- `range_types.adb` - Custom integer ranges
- `modular_types.adb` - Unsigned wraparound types
- `boolean_type.adb` - Boolean operations
- `character_type.adb` - Character type
- `enumeration_types.adb` - User-defined enumerations
- `subtypes.adb` - Constrained subtypes
- `type_conversions.adb` - Explicit type conversion

### 03_variables/ - Variables and Constants
- `variables.adb` - Variable declarations
- `constants.adb` - Named constants
- `initialization.adb` - Initialization patterns

### 04_operators/ - Operators
- `arithmetic_ops.adb` - +, -, *, /, mod, rem, **
- `relational_ops.adb` - =, /=, <, >, <=, >=
- `logical_ops.adb` - and, or, xor, not
- `string_ops.adb` - & concatenation
- `precedence.adb` - Operator precedence

### 05_control_flow/ - Control Structures
- `if_statement.adb` - If-then-else
- `case_statement.adb` - Case (switch)
- `while_loop.adb` - While loops
- `for_loop.adb` - For loops with ranges
- `basic_loop.adb` - Loop with exit
- `named_loops.adb` - Named loops and exit
- `for_of_loop.adb` - Ada 2012 iterator loops
- `goto_statement.adb` - Goto (use sparingly!)
- `null_statement.adb` - The null statement

### 06_arrays/ - Arrays
- `basic_arrays.adb` - Array basics
- `array_aggregates.adb` - Initialization syntax
- `array_attributes.adb` - 'First, 'Last, 'Range, 'Length
- `array_slices.adb` - Array slicing
- `multidim_arrays.adb` - Multi-dimensional arrays
- `unconstrained_arrays.adb` - Dynamic bounds
- `array_of_arrays.adb` - Arrays vs matrices

### 07_records/ - Records
- `basic_records.adb` - Record types
- `record_aggregates.adb` - Record initialization
- `nested_records.adb` - Records in records
- `variant_records.adb` - Discriminated records
- `record_with_array.adb` - Arrays in records

### 08_subprograms/ - Procedures and Functions
- `procedures.adb` - Procedure basics
- `functions.adb` - Function basics
- `parameter_modes.adb` - in, out, in out
- `default_parameters.adb` - Default values
- `overloading.adb` - Name overloading
- `recursion.adb` - Recursive subprograms
- `local_declarations.adb` - Local scope
- `expression_functions.adb` - Ada 2012 expression functions

### 09_packages/ - Packages
- `package_intro.adb` - Package basics
- `stack_pkg.ads/.adb` - Specification and body
- `use_stack.adb` - Using packages
- `private_types.adb` - Information hiding
- `child_packages.adb` - Hierarchical packages

### 10_exceptions/ - Exception Handling
- `basic_exceptions.adb` - Try-catch basics
- `user_exceptions.adb` - Custom exceptions
- `exception_propagation.adb` - Exception propagation
- `exception_info.adb` - Exception information
- `predefined_exceptions.adb` - Standard exceptions

### 11_access_types/ - Pointers
- `basic_access.adb` - Access type basics
- `linked_list.adb` - Dynamic data structures
- `access_parameters.adb` - Access parameters

### 12_generics/ - Generic Programming
- `generic_procedure.adb` - Generic procedures
- `generic_function.adb` - Generic functions
- `generic_package.adb` - Generic packages
- `generic_formal_params.adb` - Formal parameters

### 13_tasking/ - Concurrent Programming
- `basic_task.adb` - Task basics
- `task_types.adb` - Task types
- `rendezvous.adb` - Task communication
- `protected_objects.adb` - Shared data

### 14_attributes/ - Attributes
- `scalar_attributes.adb` - Type attributes
- `array_attrs.adb` - Array attributes
- `object_attributes.adb` - Object attributes

### 15_strings/ - String Handling
- `fixed_strings.adb` - Fixed-length strings

### 16_io/ - Input/Output
- `text_io_basics.adb` - Text I/O basics
- `integer_io.adb` - Integer I/O formatting

### 17_derived_types/ - Type Derivation
- `derived_types.adb` - Creating new types

### 18_pragmas/ - Compiler Directives
- `pragma_examples.adb` - Common pragmas

### 19_contracts/ - Contract Programming
- `contract_examples.adb` - Pre/Post conditions

### 20_applications/ - Practical Examples
- `calculator.adb` - Simple calculator
- `prime_numbers.adb` - Prime algorithms
- `sorting.adb` - Sorting algorithms
- `binary_search.adb` - Binary search
- `temperature_converter.adb` - Unit conversion
- `number_guessing.adb` - Simple game
- `morse_code.adb` - Morse encoder
- `todo_list.adb` - Task manager

---

## Z80/CP/M Considerations

These examples are designed to work with the uada80 compiler which targets the Z80/CP/M environment:

### Memory Constraints
- ~57K TPA (Transient Program Area) on 64K system
- Use small, focused examples
- Avoid large arrays when possible

### Integer Sizes
- Standard Integer is 16-bit (-32768 to 32767)
- Use `range` types to constrain values
- No native 32/64-bit without libraries

### No Floating Point Hardware
- Software floating point available but slow
- Integer arithmetic preferred
- Use fixed-point or scaled integers when possible

### Tasking
- Supported via timer interrupts
- Use protected objects for shared data
- Keep task count minimal

### I/O
- Console via BDOS function calls
- File I/O in 128-byte sectors
- 8.3 filename format

---

## How to Use

1. **Study the examples** - Each file demonstrates one concept
2. **Read the comments** - Explanations are in the code
3. **Compile and run** - Use uada80 to compile
4. **Experiment** - Modify examples to learn

### Compilation Example

```bash
# Compile an example
python -m uada80 hello_world.adb -o hello.com

# Run in CP/M emulator
cpmemu hello.com
```

---

## Learning Path

Suggested order for beginners:

1. **Basics** (01_basics/) - Start here!
2. **Types** (02_types/) - Ada's strong typing
3. **Variables** (03_variables/) - Data storage
4. **Operators** (04_operators/) - Expressions
5. **Control Flow** (05_control_flow/) - Logic
6. **Arrays** (06_arrays/) - Collections
7. **Records** (07_records/) - Structures
8. **Subprograms** (08_subprograms/) - Functions
9. **Packages** (09_packages/) - Modularity
10. **Exceptions** (10_exceptions/) - Error handling
11. **Access Types** (11_access_types/) - Pointers
12. **Generics** (12_generics/) - Templates
13. **Tasking** (13_tasking/) - Concurrency
14. **Applications** (20_applications/) - Put it together!

---

## Additional Examples Needed?

These examples cover the core language. Additional topics that could be added:
- File I/O examples specific to CP/M
- Hardware interfacing (ports, interrupts)
- More algorithms (graph, tree operations)
- Text processing utilities
- Simple games

---

## Contributing

Feel free to add more examples! Guidelines:
- One concept per file
- Extensive comments explaining the concept
- Keep examples small (Z80 memory constraints)
- Test with uada80 before submitting

---

*Created for the uada80 Ada compiler project - Ada for Z80/CP/M*
