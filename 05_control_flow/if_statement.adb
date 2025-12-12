-- Example: if_statement.adb
-- Concept: If-then-else conditional statements
--
-- Ada if statement syntax:
--   if condition then
--      statements;
--   elsif condition then    -- optional, can have multiple
--      statements;
--   else                    -- optional
--      statements;
--   end if;                 -- required (two words!)

with Ada.Text_IO;
use Ada.Text_IO;

procedure If_Statement is
   Score : Integer := 85;
   X     : Integer := 10;
   Y     : Integer := 20;
begin
   Put_Line("If statement examples:");

   -- Simple if
   if Score > 70 then
      Put_Line("  Score > 70: Passed!");
   end if;

   -- If-else
   if X > Y then
      Put_Line("  X is greater than Y");
   else
      Put_Line("  X is not greater than Y");
   end if;

   -- If-elsif-else chain
   Put_Line("Grade for score" & Integer'Image(Score) & ":");
   if Score >= 90 then
      Put_Line("  Grade: A");
   elsif Score >= 80 then
      Put_Line("  Grade: B");
   elsif Score >= 70 then
      Put_Line("  Grade: C");
   elsif Score >= 60 then
      Put_Line("  Grade: D");
   else
      Put_Line("  Grade: F");
   end if;

   -- Multiple conditions
   if X > 0 and Y > 0 then
      Put_Line("  Both X and Y are positive");
   end if;

   -- Nested if (use sparingly - prefer elsif)
   if X > 0 then
      if Y > 0 then
         Put_Line("  Both positive (nested)");
      end if;
   end if;

   -- Note: "end if" is two words, not "endif"
end If_Statement;
