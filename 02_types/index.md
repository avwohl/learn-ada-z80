---
layout: default
title: "02 - Types"
---

# Types

Ada's strong type system is a key feature for writing reliable code.

## Examples

| File | Concept |
|------|---------|
| [integer_types.adb](integer_types.adb) | Integer types and literals |
| [range_types.adb](range_types.adb) | Custom integer ranges |
| [modular_types.adb](modular_types.adb) | Unsigned wraparound types |
| [boolean_type.adb](boolean_type.adb) | Boolean operations |
| [character_type.adb](character_type.adb) | Character type |
| [enumeration_types.adb](enumeration_types.adb) | User-defined enumerations |
| [subtypes.adb](subtypes.adb) | Constrained subtypes |
| [type_conversions.adb](type_conversions.adb) | Explicit type conversion |
| [null_type.adb](null_type.adb) | Null literal and null records |

## Key Concepts

### Integer Types
```ada
type Byte is range 0 .. 255;        -- Constrained range
type Word is mod 65536;             -- Modular (wraps around)
subtype Natural is Integer range 0 .. Integer'Last;
```

### Enumeration Types
```ada
type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
type Color is (Red, Green, Blue);
```

### Type Conversion
```ada
type Meters is new Integer;
type Feet is new Integer;
M : Meters := Meters(100);    -- Explicit conversion required
```

[← Back to Index](../)
