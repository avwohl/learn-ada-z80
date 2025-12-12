---
layout: default
title: "08 - Subprograms"
---

# Subprograms

Procedures and functions in Ada.

## Examples

| File | Concept |
|------|---------|
| [procedures.adb](https://github.com/avwohl/learn-ada-z80/blob/main/08_subprograms/procedures.adb) | Procedure basics |
| [functions.adb](https://github.com/avwohl/learn-ada-z80/blob/main/08_subprograms/functions.adb) | Function basics |
| [parameter_modes.adb](https://github.com/avwohl/learn-ada-z80/blob/main/08_subprograms/parameter_modes.adb) | in, out, in out modes |
| [default_parameters.adb](https://github.com/avwohl/learn-ada-z80/blob/main/08_subprograms/default_parameters.adb) | Default parameter values |
| [named_parameters.adb](https://github.com/avwohl/learn-ada-z80/blob/main/08_subprograms/named_parameters.adb) | Named parameter association |
| [overloading.adb](https://github.com/avwohl/learn-ada-z80/blob/main/08_subprograms/overloading.adb) | Name overloading |
| [operator_overloading.adb](https://github.com/avwohl/learn-ada-z80/blob/main/08_subprograms/operator_overloading.adb) | Operator overloading |
| [recursion.adb](https://github.com/avwohl/learn-ada-z80/blob/main/08_subprograms/recursion.adb) | Recursive subprograms |
| [local_declarations.adb](https://github.com/avwohl/learn-ada-z80/blob/main/08_subprograms/local_declarations.adb) | Local scope |
| [expression_functions.adb](https://github.com/avwohl/learn-ada-z80/blob/main/08_subprograms/expression_functions.adb) | Expression functions (Ada 2012) |

## Key Concepts

### Procedures
```ada
procedure Greet(Name : String) is
begin
   Put_Line("Hello, " & Name);
end Greet;
```

### Functions
```ada
function Square(N : Integer) return Integer is
begin
   return N * N;
end Square;
```

### Parameter Modes
```ada
procedure Process(Input  : in Integer;      -- Read-only (default)
                  Output : out Integer;     -- Write-only
                  Both   : in out Integer)  -- Read-write
```

### Overloading
```ada
function Add(A, B : Integer) return Integer;
function Add(A, B, C : Integer) return Integer;
function "+"(Left, Right : Complex) return Complex;
```

[← Back to Index](../)
