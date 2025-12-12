-- Example: null_statement.adb
-- Concept: The null statement (do nothing)
--
-- "null;" is Ada's way of saying "do nothing here"
-- Useful in:
--   - Case branches that need no action
--   - Stub procedures during development
--   - When clause in exception handlers

with Ada.Text_IO;
use Ada.Text_IO;

procedure Null_Statement is
   type Priority is (Low, Medium, High, Critical);

   Level : Priority := Low;
   X     : Integer := 5;
begin
   Put_Line("Null statement examples:");

   -- In case statement when no action needed
   Put_Line("Case with null for some branches:");
   case Level is
      when Critical =>
         Put_Line("  CRITICAL: Taking immediate action!");
      when High =>
         Put_Line("  High priority: Scheduling soon");
      when Medium | Low =>
         null;  -- No action for these priorities
   end case;

   -- In if statement
   if X > 10 then
      Put_Line("  X is large");
   else
      null;  -- Explicitly do nothing
   end if;

   -- In a stub procedure (placeholder during development)
   declare
      procedure Not_Yet_Implemented is
      begin
         null;  -- TODO: implement this later
      end Not_Yet_Implemented;
   begin
      Not_Yet_Implemented;  -- Safe to call, does nothing
      Put_Line("  Called stub procedure (did nothing)");
   end;

   -- In exception handler
   begin
      declare
         N : Integer := 0;
         R : Integer;
      begin
         R := 10 / N;  -- This will raise an exception
         Put_Line("  Result:" & Integer'Image(R));
      exception
         when Constraint_Error =>
            null;  -- Silently ignore division by zero
      end;
   end;
   Put_Line("  Continued after silenced exception");

   Put_Line("null; means 'do nothing' explicitly");
end Null_Statement;
