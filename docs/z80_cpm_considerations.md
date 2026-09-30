# Z80/CP/M Considerations

These examples are designed to work with the uada80 compiler which targets the Z80/CP/M environment:

## Memory Constraints
- ~57K TPA (Transient Program Area) on 64K system
- Use small, focused examples
- Avoid large arrays when possible

## Integer Sizes
- Standard Integer is 16-bit (-32768 to 32767)
- Use `range` types to constrain values
- No native 32/64-bit without libraries

## No Floating Point Hardware
- Software floating point available but slow
- Integer arithmetic preferred
- Use fixed-point or scaled integers when possible

## Tasking
- Supported via timer interrupts
- Use protected objects for shared data
- Keep task count minimal

## I/O
- Console via BDOS function calls
- File I/O in 128-byte sectors
- 8.3 filename format
