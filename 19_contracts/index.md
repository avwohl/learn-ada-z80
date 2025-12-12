---
layout: default
title: "19 - Contracts"
---

# Contracts (Ada 2012)

## Examples

| File | Concept |
|------|---------|
| [contract_examples.adb](contract_examples.adb) | Pre/Post conditions |

## Key Concepts

```ada
function Divide(A, B : Integer) return Integer
  with Pre  => B /= 0,
       Post => Divide'Result * B <= A;

subtype Positive_Even is Integer
  with Static_Predicate => Positive_Even mod 2 = 0;
```

[← Back to Index](../)
