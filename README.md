# Ada Programming Examples for uada80 (Z80/CP/M)

This collection contains Ada example programs designed to teach Ada programming
concepts for the uada80 compiler targeting the Z80 processor running CP/M.
The collection has more than 90 example programs, one concept per file, in numbered directories from `01_basics/` to `20_applications/`.

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

1. **Basics** ([01_basics/](https://github.com/avwohl/learn-ada-z80/tree/main/01_basics)) - Start here!
2. **Types** ([02_types/](https://github.com/avwohl/learn-ada-z80/tree/main/02_types)) - Ada's strong typing
3. **Variables** ([03_variables/](https://github.com/avwohl/learn-ada-z80/tree/main/03_variables)) - Data storage
4. **Operators** ([04_operators/](https://github.com/avwohl/learn-ada-z80/tree/main/04_operators)) - Expressions
5. **Control Flow** ([05_control_flow/](https://github.com/avwohl/learn-ada-z80/tree/main/05_control_flow)) - Logic
6. **Arrays** ([06_arrays/](https://github.com/avwohl/learn-ada-z80/tree/main/06_arrays)) - Collections
7. **Records** ([07_records/](https://github.com/avwohl/learn-ada-z80/tree/main/07_records)) - Structures
8. **Subprograms** ([08_subprograms/](https://github.com/avwohl/learn-ada-z80/tree/main/08_subprograms)) - Functions
9. **Packages** ([09_packages/](https://github.com/avwohl/learn-ada-z80/tree/main/09_packages)) - Modularity
10. **Exceptions** ([10_exceptions/](https://github.com/avwohl/learn-ada-z80/tree/main/10_exceptions)) - Error handling
11. **Access Types** ([11_access_types/](https://github.com/avwohl/learn-ada-z80/tree/main/11_access_types)) - Pointers
12. **Generics** ([12_generics/](https://github.com/avwohl/learn-ada-z80/tree/main/12_generics)) - Templates
13. **Tasking** ([13_tasking/](https://github.com/avwohl/learn-ada-z80/tree/main/13_tasking)) - Concurrency
14. **Applications** ([20_applications/](https://github.com/avwohl/learn-ada-z80/tree/main/20_applications)) - Put it together!

## Documentation

- [Example programs](docs/examples.md) - every example file, by directory, with a one-line description.
- [Z80/CP/M considerations](docs/z80_cpm_considerations.md) - memory, integer sizes, floating point, tasking and I/O on the Z80.
- [Free Ada learning resources](docs/learning_resources.md) - courses, e-books, tutorials and reference material.

## Contributing

Feel free to add more examples! Guidelines:
- One concept per file
- Extensive comments explaining the concept
- Keep examples small (Z80 memory constraints)
- Test with uada80 before submitting

## License

GPL v3. See [LICENSE](LICENSE).

## Related Projects

- [80un](https://github.com/avwohl/80un) - Unpacker for the CP/M archive and compression formats LBR, ARC, squeeze, crunch, and CrLZH.
- [cpmdroid](https://github.com/avwohl/cpmdroid) - Z80/CP/M emulator for Android phones and tablets. It emulates the RomWBW HBIOS interface and a VT100 terminal.
- [cpmemu](https://github.com/avwohl/cpmemu) - Z80/CP/M emulator for Linux and Windows, with Z80 and 8080 CPU cores. It translates the BDOS and BIOS calls of CP/M 2.2 programs to the host file system.
- [ioscpm](https://github.com/avwohl/ioscpm) - Z80/CP/M emulator for iOS and macOS. It emulates the RomWBW HBIOS interface and runs CP/M 2.2 and CP/M 3.
- [mbasic](https://github.com/avwohl/mbasic) - Python interpreter for MBASIC 5.21, the Microsoft BASIC-80 for CP/M. Two compiler backends compile the programs to CP/M .COM files or to JavaScript.
- [mbasic2025](https://github.com/avwohl/mbasic2025) - Reconstruction of the lost source code of MBASIC 5.21, the Microsoft BASIC-80 for CP/M. The MACRO-80 source code assembles to a binary that matches mbasic.com byte for byte.
- [mbasicc](https://github.com/avwohl/mbasicc) - C++17 interpreter for MBASIC 5.21, the Microsoft BASIC-80 for CP/M. It runs on Linux and macOS.
- [mbasicc_web](https://github.com/avwohl/mbasicc_web) - Web browser interpreter for MBASIC 5.21, the Microsoft BASIC-80 for CP/M. Emscripten compiles the mbasicc interpreter to WebAssembly.
- [mpm2](https://github.com/avwohl/mpm2) - Z80 emulator for MP/M II, the multi-user CP/M operating system. Users connect over SSH, and SFTP clients transfer files.
- [romwbw_emu](https://github.com/avwohl/romwbw_emu) - Hardware-level Z80/CP/M emulator for Linux and macOS. It emulates the RomWBW HBIOS interface and switches banks in 512 KB of ROM and 512 KB of RAM.
- [scelbal](https://github.com/avwohl/scelbal) - Floating-point BASIC interpreter for the 8080 processor and CP/M. A translator converts the original 8008 source code to 8080 source code.
- [uada80](https://github.com/avwohl/uada80) - Ada compiler for the Z80 processor and CP/M 2.2. It compiles a subset of Ada 2012 to CP/M .COM files.
- [uc80](https://github.com/avwohl/uc80) - C compiler for the Z80 processor and CP/M. It optimizes for small code size.
- [ucow](https://github.com/avwohl/ucow) - Cowgol compiler for the Z80 processor and CP/M. It runs on Linux in Python.
- [um80_and_friends](https://github.com/avwohl/um80_and_friends) - Linux toolchain that is compatible with Microsoft MACRO-80. It has an assembler, a linker, a librarian, and a disassembler.
- [upeepz80](https://github.com/avwohl/upeepz80) - Peephole optimizer for Z80 compilers that write lowercase Z80 assembly language. It shortens jumps to jr, builds djnz loops, and removes dead stores.
- [uplm80](https://github.com/avwohl/uplm80) - PL/M-80 compiler for the Z80 processor and CP/M. It writes Intel 8080 and Zilog Z80 assembly language.
- [z80cpmw](https://github.com/avwohl/z80cpmw) - Z80/CP/M emulator for Windows. It emulates the RomWBW HBIOS interface and boots CP/M from disk images.

