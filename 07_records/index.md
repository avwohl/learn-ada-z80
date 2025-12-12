---
layout: default
title: "07 - Records"
---

# Records

Structured data types in Ada.

## Examples

| File | Concept |
|------|---------|
| [basic_records.adb](basic_records.adb) | Record types |
| [record_aggregates.adb](record_aggregates.adb) | Record initialization |
| [nested_records.adb](nested_records.adb) | Records in records |
| [variant_records.adb](variant_records.adb) | Discriminated records |
| [record_with_array.adb](record_with_array.adb) | Arrays in records |

## Key Concepts

```ada
type Point is record
   X, Y : Integer;
end record;

type Person is record
   Name : String(1..20);
   Age  : Natural;
end record;

P := (X => 10, Y => 20);
P.X := 30;
```

[← Back to Index](../)
