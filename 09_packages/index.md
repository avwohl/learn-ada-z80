---
layout: default
title: "09 - Packages"
---

# Packages

Modular programming in Ada.

## Examples

| File | Concept |
|------|---------|
| [package_intro.adb](package_intro.adb) | Package basics |
| [stack_pkg.ads](stack_pkg.ads) | Package specification |
| [stack_pkg.adb](stack_pkg.adb) | Package body |
| [use_stack.adb](use_stack.adb) | Using packages |
| [private_types.adb](private_types.adb) | Information hiding |
| [child_packages.adb](child_packages.adb) | Hierarchical packages |

## Key Concepts

```ada
-- Specification (.ads)
package Stack is
   procedure Push(Item : Integer);
   function Pop return Integer;
private
   -- Hidden implementation
end Stack;

-- Body (.adb)
package body Stack is
   -- Implementation
end Stack;

-- Usage
with Stack;
Stack.Push(42);
```

[← Back to Index](../)
