-- Example: identifiers.adb
-- Concept: Ada identifier naming rules
--
-- Ada identifiers:
--   - Start with a letter
--   - Can contain letters, digits, and underscores
--   - Cannot end with underscore
--   - Cannot have consecutive underscores
--   - Are case-insensitive (Hello = HELLO = hello)
--   - Should use Capitalized_Words style by convention

with Ada.Text_IO;
use Ada.Text_IO;

procedure Identifiers is
   -- Valid identifiers
   Count        : Integer := 0;
   Total_Sum    : Integer := 100;
   X2           : Integer := 2;
   My_Variable  : Integer := 42;

   -- These would be INVALID:
   -- 2Start    : Integer;  -- Can't start with digit
   -- My__Var   : Integer;  -- No consecutive underscores
   -- End_      : Integer;  -- Can't end with underscore

   -- Case doesn't matter - these are the same:
   -- count, COUNT, Count, cOuNt all refer to Count above
begin
   Put_Line("Count = " & Integer'Image(Count));
   Put_Line("Total_Sum = " & Integer'Image(Total_Sum));
   Put_Line("X2 = " & Integer'Image(X2));
   Put_Line("My_Variable = " & Integer'Image(My_Variable));
end Identifiers;
