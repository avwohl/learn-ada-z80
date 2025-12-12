---
layout: default
title: "01 - Basics"
---

# Basics

Getting started with Ada programming.

## Examples

| File | Concept |
|------|---------|
| [hello_world.adb](hello_world.adb) | The simplest Ada program |
| [hello_use.adb](hello_use.adb) | Using "use" clauses |
| [comments.adb](comments.adb) | Comment syntax |
| [identifiers.adb](identifiers.adb) | Naming rules |
| [reserved_words.adb](reserved_words.adb) | Ada keywords |
| [program_structure.adb](program_structure.adb) | Program organization |
| [semicolons.adb](semicolons.adb) | Statement terminators |
| [blocks.adb](blocks.adb) | Declare blocks |

## Key Concepts

### Program Structure
```ada
with Ada.Text_IO;    -- Import package
use Ada.Text_IO;     -- Make names visible

procedure My_Program is
   -- Declarations here
begin
   -- Statements here
end My_Program;
```

### Comments
```ada
-- This is a comment (only single-line style)
X : Integer := 42;  -- End of line comment
```

### Reserved Words
Ada has many keywords including: `begin`, `end`, `if`, `then`, `else`, `loop`, `for`, `while`, `case`, `when`, `procedure`, `function`, `package`, `type`, `is`, `in`, `out`, `and`, `or`, `not`, `null`, `return`, `exit`, `declare`, `exception`, `raise`, `with`, `use`, `constant`, `array`, `record`, `access`, `task`, `protected`, `generic`, and more.

[← Back to Index](../)
