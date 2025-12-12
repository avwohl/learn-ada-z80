---
layout: default
title: "11 - Access Types"
---

# Access Types (Pointers)

Dynamic memory and pointers in Ada.

## Examples

| File | Concept |
|------|---------|
| [basic_access.adb](https://github.com/avwohl/learn-ada-z80/blob/main/11_access_types/basic_access.adb) | Access type basics |
| [linked_list.adb](https://github.com/avwohl/learn-ada-z80/blob/main/11_access_types/linked_list.adb) | Dynamic data structures |
| [access_parameters.adb](https://github.com/avwohl/learn-ada-z80/blob/main/11_access_types/access_parameters.adb) | Access parameters |

## Key Concepts

```ada
type Int_Ptr is access Integer;
P : Int_Ptr := new Integer'(42);
Put(P.all);  -- Dereference

type Node;
type Node_Ptr is access Node;
type Node is record
   Data : Integer;
   Next : Node_Ptr;
end record;
```

[← Back to Index](../)
