-- Example: relational_ops.adb
-- Concept: Relational (comparison) operators
--
-- Ada relational operators:
--   =    Equal
--   /=   Not equal
--   <    Less than
--   >    Greater than
--   <=   Less than or equal
--   >=   Greater than or equal
-- All return Boolean (True or False)

with Ada.Text_IO;
use Ada.Text_IO;

procedure Relational_Ops is
   A : Integer := 10;
   B : Integer := 20;
   C : Integer := 10;

   X : Character := 'A';
   Y : Character := 'B';

   P : Boolean;
begin
   Put_Line("Relational operators (A=10, B=20, C=10):");

   P := A = B;
   Put_Line("  A = B is " & Boolean'Image(P));

   P := A = C;
   Put_Line("  A = C is " & Boolean'Image(P));

   P := A /= B;
   Put_Line("  A /= B is " & Boolean'Image(P));

   P := A < B;
   Put_Line("  A < B is " & Boolean'Image(P));

   P := A > B;
   Put_Line("  A > B is " & Boolean'Image(P));

   P := A <= C;
   Put_Line("  A <= C is " & Boolean'Image(P));

   P := A >= B;
   Put_Line("  A >= B is " & Boolean'Image(P));

   -- Character comparisons (by ASCII value)
   Put_Line("Character comparisons (X='A', Y='B'):");
   Put_Line("  X < Y is " & Boolean'Image(X < Y));
   Put_Line("  X = 'A' is " & Boolean'Image(X = 'A'));

   -- Chained comparisons require explicit logic
   -- Cannot write: A < B < C  (must use "and")
   P := A < B and B < 30;
   Put_Line("  A < B and B < 30 is " & Boolean'Image(P));

   -- Range membership test
   P := A in 1 .. 100;
   Put_Line("  A in 1..100 is " & Boolean'Image(P));

   P := A not in 50 .. 100;
   Put_Line("  A not in 50..100 is " & Boolean'Image(P));
end Relational_Ops;
