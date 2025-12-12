---
layout: default
title: "13 - Tasking"
---

# Tasking (Concurrency)

Concurrent programming in Ada.

## Examples

| File | Concept |
|------|---------|
| [basic_task.adb](basic_task.adb) | Task basics |
| [task_types.adb](task_types.adb) | Task types |
| [rendezvous.adb](rendezvous.adb) | Task communication |
| [protected_objects.adb](protected_objects.adb) | Shared data |

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
