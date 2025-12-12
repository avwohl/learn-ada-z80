-- Example: basic_exceptions.adb
-- Concept: Exception handling basics
--
-- Exceptions handle runtime errors gracefully.
-- Ada has predefined exceptions and you can define your own.
-- Use "raise" to throw, "exception when" to catch.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Basic_Exceptions is
   A, B, Result : Integer;
begin
   Put_Line("Basic exception handling:");

   -- Division by zero raises Constraint_Error
   A := 10;
   B := 0;

   Put_Line("Attempting 10 / 0:");
   begin
      Result := A / B;
      Put_Line("Result:" & Integer'Image(Result));  -- Won't reach here
   exception
      when Constraint_Error =>
         Put_Line("  Caught Constraint_Error: division by zero!");
   end;

   -- Array bounds checking
   Put_Line("Attempting out-of-bounds access:");
   declare
      Arr : array(1 .. 5) of Integer := (others => 0);
   begin
      Arr(10) := 100;  -- Index out of bounds
   exception
      when Constraint_Error =>
         Put_Line("  Caught Constraint_Error: index out of bounds!");
   end;

   -- Range overflow
   Put_Line("Attempting range overflow:");
   declare
      subtype Small is Integer range 1 .. 10;
      S : Small;
   begin
      S := 100;  -- Out of range
   exception
      when Constraint_Error =>
         Put_Line("  Caught Constraint_Error: range overflow!");
   end;

   -- Multiple exception handlers
   Put_Line("Multiple handlers:");
   begin
      Result := A / B;
   exception
      when Constraint_Error =>
         Put_Line("  Constraint_Error handler");
      when Storage_Error =>
         Put_Line("  Storage_Error handler");
      when others =>
         Put_Line("  Catch-all handler");
   end;

   Put_Line("Program continues after handled exceptions.");
end Basic_Exceptions;
