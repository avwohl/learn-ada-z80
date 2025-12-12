-- Example: predefined_exceptions.adb
-- Concept: Ada's predefined exceptions
--
-- Ada defines several standard exceptions:
--   Constraint_Error - range violations, null access, etc.
--   Program_Error - impossible situations
--   Storage_Error - out of memory
--   Tasking_Error - task communication failures
--   Data_Error - invalid input (I/O)
--   End_Error - read past end of file

with Ada.Text_IO;
use Ada.Text_IO;

procedure Predefined_Exceptions is
begin
   Put_Line("Predefined exception examples:");

   -- Constraint_Error: numeric overflow
   Put_Line("1. Numeric overflow:");
   begin
      declare
         type Small is range 0 .. 100;
         S : Small := 200;  -- Out of range
      begin
         null;
      end;
   exception
      when Constraint_Error =>
         Put_Line("   Caught Constraint_Error (range)");
   end;

   -- Constraint_Error: division by zero
   Put_Line("2. Division by zero:");
   begin
      declare
         X : Integer := 10 / 0;
      begin
         null;
      end;
   exception
      when Constraint_Error =>
         Put_Line("   Caught Constraint_Error (div zero)");
   end;

   -- Constraint_Error: array bounds
   Put_Line("3. Array bounds:");
   begin
      declare
         A : array(1..3) of Integer := (1, 2, 3);
         X : Integer;
      begin
         X := A(5);
      end;
   exception
      when Constraint_Error =>
         Put_Line("   Caught Constraint_Error (bounds)");
   end;

   -- Constraint_Error: null access
   Put_Line("4. Null access:");
   begin
      declare
         type Int_Ptr is access Integer;
         P : Int_Ptr := null;
         X : Integer;
      begin
         X := P.all;  -- Dereference null
      end;
   exception
      when Constraint_Error =>
         Put_Line("   Caught Constraint_Error (null access)");
   end;

   -- Program_Error: missing return
   Put_Line("5. Missing return:");
   begin
      declare
         function Bad return Integer is
         begin
            null;  -- No return statement!
         end Bad;
         X : Integer;
      begin
         X := Bad;
      end;
   exception
      when Program_Error =>
         Put_Line("   Caught Program_Error (no return)");
   end;

   -- Constraint_Error: string length mismatch
   Put_Line("6. String length mismatch:");
   begin
      declare
         S : String(1..5);
      begin
         S := "Hello World";  -- Too long
      end;
   exception
      when Constraint_Error =>
         Put_Line("   Caught Constraint_Error (string length)");
   end;

   Put_Line("All predefined exception tests complete.");
end Predefined_Exceptions;
