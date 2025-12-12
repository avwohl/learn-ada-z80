-- Example: arithmetic_ops.adb
-- Concept: Arithmetic operators in Ada
--
-- Ada arithmetic operators:
--   +    Addition
--   -    Subtraction (and unary minus)
--   *    Multiplication
--   /    Integer division (truncates toward zero)
--   mod  Modulus (sign of result = sign of divisor)
--   rem  Remainder (sign of result = sign of dividend)
--   **   Exponentiation
--   abs  Absolute value

with Ada.Text_IO;
use Ada.Text_IO;

procedure Arithmetic_Ops is
   A : Integer := 17;
   B : Integer := 5;
   C : Integer := -17;
   R : Integer;
begin
   Put_Line("Arithmetic operators (A=17, B=5, C=-17):");

   -- Basic operations
   R := A + B;
   Put_Line("  A + B =" & Integer'Image(R));

   R := A - B;
   Put_Line("  A - B =" & Integer'Image(R));

   R := A * B;
   Put_Line("  A * B =" & Integer'Image(R));

   R := A / B;
   Put_Line("  A / B =" & Integer'Image(R) & " (integer division)");

   -- mod vs rem with positive numbers
   R := A mod B;
   Put_Line("  A mod B =" & Integer'Image(R));

   R := A rem B;
   Put_Line("  A rem B =" & Integer'Image(R));

   -- mod vs rem with negative dividend
   Put_Line("Negative number examples (C=-17):");
   R := C mod B;
   Put_Line("  C mod B =" & Integer'Image(R) & " (sign of divisor)");

   R := C rem B;
   Put_Line("  C rem B =" & Integer'Image(R) & " (sign of dividend)");

   -- Exponentiation
   R := 2 ** 8;
   Put_Line("  2 ** 8 =" & Integer'Image(R));

   -- Absolute value
   R := abs(C);
   Put_Line("  abs(C) =" & Integer'Image(R));

   -- Unary minus
   R := -A;
   Put_Line("  -A =" & Integer'Image(R));

   -- Compound expressions
   R := (A + B) * 2;
   Put_Line("  (A + B) * 2 =" & Integer'Image(R));
end Arithmetic_Ops;
