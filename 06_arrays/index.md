---
layout: default
title: "06 - Arrays"
---

# Arrays

Array types and operations in Ada.

## Examples

| File | Concept |
|------|---------|
| [basic_arrays.adb](basic_arrays.adb) | Array basics |
| [array_aggregates.adb](array_aggregates.adb) | Initialization syntax |
| [array_attributes.adb](array_attributes.adb) | 'First, 'Last, 'Range, 'Length |
| [array_slices.adb](array_slices.adb) | Array slicing |
| [multidim_arrays.adb](multidim_arrays.adb) | Multi-dimensional arrays |
| [unconstrained_arrays.adb](unconstrained_arrays.adb) | Dynamic bounds |
| [array_of_arrays.adb](array_of_arrays.adb) | Arrays vs matrices |

## Key Concepts

```ada
type Vector is array(1..10) of Integer;
type Matrix is array(1..3, 1..4) of Integer;

-- Aggregates
A := (1, 2, 3, 4, 5);
B := (1 => 10, others => 0);

-- Attributes
for I in A'Range loop ...
A'First, A'Last, A'Length

-- Slices
A(3..7) := B(1..5);
```

[← Back to Index](../)
