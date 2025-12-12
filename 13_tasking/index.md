---
layout: default
title: "13 - Tasking"
---

# Tasking (Concurrency)

Concurrent programming in Ada.

## Examples

| File | Concept |
|------|---------|
| [basic_task.adb](https://github.com/avwohl/learn-ada-z80/blob/main/13_tasking/basic_task.adb) | Task basics |
| [task_types.adb](https://github.com/avwohl/learn-ada-z80/blob/main/13_tasking/task_types.adb) | Task types |
| [rendezvous.adb](https://github.com/avwohl/learn-ada-z80/blob/main/13_tasking/rendezvous.adb) | Task communication |
| [protected_objects.adb](https://github.com/avwohl/learn-ada-z80/blob/main/13_tasking/protected_objects.adb) | Shared data |

## Key Concepts

```ada
task Worker;
task body Worker is
begin
   -- Runs concurrently
end Worker;

protected Counter is
   procedure Increment;
   function Value return Integer;
private
   Count : Integer := 0;
end Counter;
```

*Note: On Z80/CP/M, tasking requires runtime timer interrupt support.*

[← Back to Index](../)
