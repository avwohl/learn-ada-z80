---
layout: default
title: "10 - Exceptions"
---

# Exception Handling

Error handling in Ada.

## Examples

| File | Concept |
|------|---------|
| [basic_exceptions.adb](https://github.com/avwohl/learn-ada-z80/blob/main/10_exceptions/basic_exceptions.adb) | Try-catch basics |
| [user_exceptions.adb](https://github.com/avwohl/learn-ada-z80/blob/main/10_exceptions/user_exceptions.adb) | Custom exceptions |
| [exception_propagation.adb](https://github.com/avwohl/learn-ada-z80/blob/main/10_exceptions/exception_propagation.adb) | Exception propagation |
| [exception_info.adb](https://github.com/avwohl/learn-ada-z80/blob/main/10_exceptions/exception_info.adb) | Exception information |
| [predefined_exceptions.adb](https://github.com/avwohl/learn-ada-z80/blob/main/10_exceptions/predefined_exceptions.adb) | Standard exceptions |

## Key Concepts

```ada
My_Error : exception;

begin
   raise My_Error with "message";
exception
   when Constraint_Error =>
      Put_Line("Range error");
   when E : others =>
      Put_Line(Exception_Message(E));
end;
```

[← Back to Index](../)
