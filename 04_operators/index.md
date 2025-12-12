---
layout: default
title: "04 - Operators"
---

# Operators

Ada operators and expressions.

## Examples

| File | Concept |
|------|---------|
| [arithmetic_ops.adb](https://github.com/avwohl/learn-ada-z80/blob/main/04_operators/arithmetic_ops.adb) | +, -, *, /, mod, rem, ** |
| [relational_ops.adb](https://github.com/avwohl/learn-ada-z80/blob/main/04_operators/relational_ops.adb) | =, /=, <, >, <=, >= |
| [logical_ops.adb](https://github.com/avwohl/learn-ada-z80/blob/main/04_operators/logical_ops.adb) | and, or, xor, not |
| [string_ops.adb](https://github.com/avwohl/learn-ada-z80/blob/main/04_operators/string_ops.adb) | & concatenation |
| [precedence.adb](https://github.com/avwohl/learn-ada-z80/blob/main/04_operators/precedence.adb) | Operator precedence |

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
