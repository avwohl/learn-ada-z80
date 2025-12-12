---
layout: default
title: "07 - Records"
---

# Records

Structured data types in Ada.

## Examples

| File | Concept |
|------|---------|
| [basic_records.adb](https://github.com/avwohl/learn-ada-z80/blob/main/07_records/basic_records.adb) | Record types |
| [record_aggregates.adb](https://github.com/avwohl/learn-ada-z80/blob/main/07_records/record_aggregates.adb) | Record initialization |
| [nested_records.adb](https://github.com/avwohl/learn-ada-z80/blob/main/07_records/nested_records.adb) | Records in records |
| [variant_records.adb](https://github.com/avwohl/learn-ada-z80/blob/main/07_records/variant_records.adb) | Discriminated records |
| [record_with_array.adb](https://github.com/avwohl/learn-ada-z80/blob/main/07_records/record_with_array.adb) | Arrays in records |

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
