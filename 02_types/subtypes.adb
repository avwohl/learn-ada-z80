-- Example: subtypes.adb
-- Concept: Subtypes - constrained versions of types
--
-- A subtype is the same type with additional constraints.
-- Values of a subtype can be used wherever the parent type is expected.
-- Unlike derived types, subtypes are compatible with their parent.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Subtypes is
   -- Subtypes of Integer with range constraints
   subtype Small_Int is Integer range -100 .. 100;
   subtype Score is Integer range 0 .. 100;
   subtype Percent is Integer range 0 .. 100;

   -- Built-in subtypes: Natural and Positive are subtypes of Integer
   -- subtype Natural is Integer range 0 .. Integer'Last;
   -- subtype Positive is Integer range 1 .. Integer'Last;

   A : Integer := 50;
   B : Small_Int := 50;
   C : Score := 85;
   D : Percent := 75;
begin
   Put_Line("Subtype demonstration:");
   Put_Line("  A (Integer) =" & Integer'Image(A));
   Put_Line("  B (Small_Int) =" & Integer'Image(B));
   Put_Line("  C (Score) =" & Integer'Image(C));
   Put_Line("  D (Percent) =" & Integer'Image(D));

   -- Subtypes are compatible - can assign between them
   A := B;  -- OK: Small_Int is subtype of Integer
   B := C;  -- OK: both constrained subtypes of Integer

   Put_Line("After assignments:");
   Put_Line("  A =" & Integer'Image(A));
   Put_Line("  B =" & Integer'Image(B));

   -- Show subtype bounds
   Put_Line("Small_Int range:" &
            Small_Int'Image(Small_Int'First) & " .." &
            Small_Int'Image(Small_Int'Last));

   -- Range check happens at runtime
   -- B := 200;  -- Would raise Constraint_Error (out of range)

   Put_Line("Subtypes share the same base type but add constraints");
end Subtypes;
