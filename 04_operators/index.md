---
layout: default
title: "04 - Operators"
---

# Operators

Ada operators and expressions.

## Examples

| File | Concept |
|------|---------|
| [arithmetic_ops.adb](arithmetic_ops.adb) | +, -, *, /, mod, rem, ** |
| [relational_ops.adb](relational_ops.adb) | =, /=, <, >, <=, >= |
| [logical_ops.adb](logical_ops.adb) | and, or, xor, not |
| [string_ops.adb](string_ops.adb) | & concatenation |
| [precedence.adb](precedence.adb) | Operator precedence |

## Key Concepts

```ada
-- Arithmetic
A + B, A - B, A * B, A / B
A mod B  -- Modulus (sign of divisor)
A rem B  -- Remainder (sign of dividend)
A ** 2   -- Exponentiation

-- Relational (return Boolean)
A = B, A /= B, A < B, A <= B

-- Logical
A and B, A or B, not A
A and then B  -- Short-circuit
```

[← Back to Index](../)
