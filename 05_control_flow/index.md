---
layout: default
title: "05 - Control Flow"
---

# Control Flow

Conditional statements and loops in Ada.

## Examples

| File | Concept |
|------|---------|
| [if_statement.adb](if_statement.adb) | If-then-else |
| [if_expression.adb](if_expression.adb) | If expressions (Ada 2012) |
| [case_statement.adb](case_statement.adb) | Case (switch) statements |
| [case_expression.adb](case_expression.adb) | Case expressions (Ada 2012) |
| [while_loop.adb](while_loop.adb) | While loops |
| [for_loop.adb](for_loop.adb) | For loops with ranges |
| [for_of_loop.adb](for_of_loop.adb) | For-of iterator loops (Ada 2012) |
| [basic_loop.adb](basic_loop.adb) | Loop with exit |
| [named_loops.adb](named_loops.adb) | Named loops |
| [goto_statement.adb](goto_statement.adb) | Goto (use sparingly!) |
| [null_statement.adb](null_statement.adb) | The null statement |

## Key Concepts

### If Statement
```ada
if X > 0 then
   Put_Line("Positive");
elsif X < 0 then
   Put_Line("Negative");
else
   Put_Line("Zero");
end if;
```

### Case Statement
```ada
case Day is
   when Mon .. Fri => Put_Line("Weekday");
   when Sat | Sun  => Put_Line("Weekend");
end case;
```

### Loops
```ada
-- For loop
for I in 1 .. 10 loop
   Put(Integer'Image(I));
end loop;

-- While loop
while X > 0 loop
   X := X - 1;
end loop;

-- Basic loop with exit
loop
   exit when Done;
end loop;
```

### Ada 2012 Expressions
```ada
Max := (if A > B then A else B);
Name := (case Day is when Mon => "Monday", when Tue => "Tuesday", ...);
```

[← Back to Index](../)
