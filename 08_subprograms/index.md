---
layout: default
title: "08 - Subprograms"
---

# Subprograms

Procedures and functions in Ada.

## Examples

| File | Concept |
|------|---------|
| [procedures.adb](procedures.adb) | Procedure basics |
| [functions.adb](functions.adb) | Function basics |
| [parameter_modes.adb](parameter_modes.adb) | in, out, in out modes |
| [default_parameters.adb](default_parameters.adb) | Default parameter values |
| [named_parameters.adb](named_parameters.adb) | Named parameter association |
| [overloading.adb](overloading.adb) | Name overloading |
| [operator_overloading.adb](operator_overloading.adb) | Operator overloading |
| [recursion.adb](recursion.adb) | Recursive subprograms |
| [local_declarations.adb](local_declarations.adb) | Local scope |
| [expression_functions.adb](expression_functions.adb) | Expression functions (Ada 2012) |

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
