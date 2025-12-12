---
layout: default
title: "03 - Variables"
---

# Variables and Constants

Data storage in Ada.

## Examples

| File | Concept |
|------|---------|
| [variables.adb](https://github.com/avwohl/learn-ada-z80/blob/main/03_variables/variables.adb) | Variable declarations |
| [constants.adb](https://github.com/avwohl/learn-ada-z80/blob/main/03_variables/constants.adb) | Named constants |
| [initialization.adb](https://github.com/avwohl/learn-ada-z80/blob/main/03_variables/initialization.adb) | Initialization patterns |

## Key Concepts

```ada
-- Variables
Count : Integer := 0;
Name  : String(1..20);

-- Constants
Max_Size : constant := 100;
Pi       : constant := 3.14159;

-- Multiple declarations
X, Y, Z : Integer := 0;
```

[← Back to Index](../)
