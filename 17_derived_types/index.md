---
layout: default
title: "17 - Derived Types"
---

# Derived Types

## Examples

| File | Concept |
|------|---------|
| [derived_types.adb](derived_types.adb) | Type derivation |

## Key Concepts

```ada
type Meters is new Integer;
type Feet is new Integer;
-- Cannot accidentally mix!
M : Meters := Meters(F);  -- Explicit conversion
```

[← Back to Index](../)
