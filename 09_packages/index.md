---
layout: default
title: "09 - Packages"
---

# Packages

Modular programming in Ada.

## Examples

| File | Concept |
|------|---------|
| [package_intro.adb](https://github.com/avwohl/learn-ada-z80/blob/main/09_packages/package_intro.adb) | Package basics |
| [stack_pkg.ads](https://github.com/avwohl/learn-ada-z80/blob/main/09_packages/stack_pkg.ads) | Package specification |
| [stack_pkg.adb](https://github.com/avwohl/learn-ada-z80/blob/main/09_packages/stack_pkg.adb) | Package body |
| [use_stack.adb](https://github.com/avwohl/learn-ada-z80/blob/main/09_packages/use_stack.adb) | Using packages |
| [private_types.adb](https://github.com/avwohl/learn-ada-z80/blob/main/09_packages/private_types.adb) | Information hiding |
| [child_packages.adb](https://github.com/avwohl/learn-ada-z80/blob/main/09_packages/child_packages.adb) | Hierarchical packages |

## Key Concepts

```ada
-- Specification https://github.com/avwohl/learn-ada-z80/blob/main/09_packages/.ads)
package Stack is
   procedure Push(Item : Integer);
   function Pop return Integer;
private
   -- Hidden implementation
end Stack;

-- Body https://github.com/avwohl/learn-ada-z80/blob/main/09_packages/.adb)
package body Stack is
   -- Implementation
end Stack;

-- Usage
with Stack;
Stack.Push(42);
```

[← Back to Index](../)
