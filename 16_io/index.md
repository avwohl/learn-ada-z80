---
layout: default
title: "16 - I/O"
---

# Input/Output

## Examples

| File | Concept |
|------|---------|
| [text_io_basics.adb](https://github.com/avwohl/learn-ada-z80/blob/main/16_io/text_io_basics.adb) | Text I/O basics |
| [integer_io.adb](https://github.com/avwohl/learn-ada-z80/blob/main/16_io/integer_io.adb) | Integer I/O formatting |

## Key Concepts

```ada
with Ada.Text_IO; use Ada.Text_IO;
Put("Hello");
Put_Line("World");
New_Line;

package Int_IO is new Integer_IO(Integer);
Int_IO.Put(42, Width => 5);
```

[← Back to Index](../)
