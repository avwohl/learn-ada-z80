---
layout: default
title: "12 - Generics"
---

# Generic Programming

Parametric polymorphism in Ada.

## Examples

| File | Concept |
|------|---------|
| [generic_procedure.adb](https://github.com/avwohl/learn-ada-z80/blob/main/12_generics/generic_procedure.adb) | Generic procedures |
| [generic_function.adb](https://github.com/avwohl/learn-ada-z80/blob/main/12_generics/generic_function.adb) | Generic functions |
| [generic_package.adb](https://github.com/avwohl/learn-ada-z80/blob/main/12_generics/generic_package.adb) | Generic packages |
| [generic_formal_params.adb](https://github.com/avwohl/learn-ada-z80/blob/main/12_generics/generic_formal_params.adb) | Formal parameters |

## Key Concepts

```ada
generic
   type Element is private;
procedure Swap(A, B : in out Element);

procedure Swap(A, B : in Out Element) is
   Temp : Element := A;
begin
   A := B; B := Temp;
end Swap;

procedure Swap_Int is new Swap(Integer);
```

[← Back to Index](../)
