-- Example: boolean_type.adb
-- Concept: Boolean type and logical operations
--
-- Boolean is a built-in enumeration type with values:
--   False, True
-- Ada uses short-circuit evaluation with "and then" / "or else"

with Ada.Text_IO;
use Ada.Text_IO;

procedure Boolean_Type is
   A : Boolean := True;
   B : Boolean := False;
   C : Boolean;

   X : Integer := 10;
   Y : Integer := 20;
begin
   Put_Line("Boolean values:");
   Put_Line("  A = " & Boolean'Image(A));
   Put_Line("  B = " & Boolean'Image(B));

   Put_Line("Logical operators:");
   Put_Line("  A and B = " & Boolean'Image(A and B));
   Put_Line("  A or B = " & Boolean'Image(A or B));
   Put_Line("  A xor B = " & Boolean'Image(A xor B));
   Put_Line("  not A = " & Boolean'Image(not A));

   -- Comparison operators return Boolean
   Put_Line("Comparisons (X=10, Y=20):");
   Put_Line("  X = Y is " & Boolean'Image(X = Y));
   Put_Line("  X /= Y is " & Boolean'Image(X /= Y));  -- Not equal
   Put_Line("  X < Y is " & Boolean'Image(X < Y));
   Put_Line("  X >= Y is " & Boolean'Image(X >= Y));

   -- Short-circuit evaluation
   -- "and then" only evaluates right side if left is True
   -- "or else" only evaluates right side if left is False
   C := (X > 0) and then (Y / X > 1);  -- Safe: won't divide if X=0
   Put_Line("Short-circuit result: " & Boolean'Image(C));

   -- Boolean in conditions
   if A then
      Put_Line("A is True");
   end if;

   if not B then
      Put_Line("B is False");
   end if;
end Boolean_Type;
