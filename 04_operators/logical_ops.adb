-- Example: logical_ops.adb
-- Concept: Logical (Boolean) operators
--
-- Ada logical operators:
--   and       Logical AND (evaluates both sides)
--   or        Logical OR (evaluates both sides)
--   xor       Exclusive OR
--   not       Logical NOT
--   and then  Short-circuit AND (stops if left is False)
--   or else   Short-circuit OR (stops if left is True)

with Ada.Text_IO;
use Ada.Text_IO;

procedure Logical_Ops is
   A : Boolean := True;
   B : Boolean := False;
   C : Boolean := True;
   R : Boolean;

   X : Integer := 10;
   Y : Integer := 0;
begin
   Put_Line("Logical operators (A=True, B=False, C=True):");

   R := A and B;
   Put_Line("  A and B = " & Boolean'Image(R));

   R := A and C;
   Put_Line("  A and C = " & Boolean'Image(R));

   R := A or B;
   Put_Line("  A or B = " & Boolean'Image(R));

   R := B or B;
   Put_Line("  B or B = " & Boolean'Image(R));

   R := A xor B;
   Put_Line("  A xor B = " & Boolean'Image(R));

   R := A xor C;
   Put_Line("  A xor C = " & Boolean'Image(R));

   R := not A;
   Put_Line("  not A = " & Boolean'Image(R));

   R := not B;
   Put_Line("  not B = " & Boolean'Image(R));

   -- Short-circuit evaluation
   Put_Line("Short-circuit operators:");

   -- "and then" stops if left side is False
   R := B and then A;  -- Won't evaluate A since B is False
   Put_Line("  B and then A = " & Boolean'Image(R));

   -- "or else" stops if left side is True
   R := A or else B;  -- Won't evaluate B since A is True
   Put_Line("  A or else B = " & Boolean'Image(R));

   -- Short-circuit prevents runtime errors
   -- This is safe because Y=0 case is handled first
   if Y /= 0 and then X / Y > 0 then
      Put_Line("  Division result positive");
   else
      Put_Line("  Avoided division by zero with 'and then'");
   end if;

   -- Combining operators (precedence: not > and > or)
   R := A and B or C;  -- Same as (A and B) or C
   Put_Line("  A and B or C = " & Boolean'Image(R));

   R := A and (B or C);  -- Parentheses change precedence
   Put_Line("  A and (B or C) = " & Boolean'Image(R));
end Logical_Ops;
