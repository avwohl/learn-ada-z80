-- Example: semicolons.adb
-- Concept: Semicolons in Ada are statement terminators
--
-- In Ada, semicolons terminate statements (not separate them).
-- This means every statement ends with a semicolon,
-- including the last one before "end".
-- This differs from languages where ; separates statements.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Semicolons is
   A : Integer := 1;   -- Declaration ends with ;
   B : Integer := 2;   -- Every declaration needs one
   C : Integer;        -- Even the last declaration
begin
   C := A + B;         -- Assignment ends with ;
   Put_Line("A =" & Integer'Image(A));  -- Procedure call ends with ;
   Put_Line("B =" & Integer'Image(B));  -- Every statement
   Put_Line("C =" & Integer'Image(C));  -- Including the last one
end Semicolons;  -- The final "end" also needs a semicolon
