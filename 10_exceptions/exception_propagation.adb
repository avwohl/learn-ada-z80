-- Example: exception_propagation.adb
-- Concept: Exception propagation
--
-- Unhandled exceptions propagate up the call stack.
-- Each level can handle, re-raise, or let it propagate.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Exception_Propagation is

   Calculation_Error : exception;

   procedure Level_3 is
   begin
      Put_Line("  Entering Level_3");
      Put_Line("  Raising Calculation_Error in Level_3");
      raise Calculation_Error;
      Put_Line("  This line never executes");
   end Level_3;

   procedure Level_2 is
   begin
      Put_Line(" Entering Level_2");
      Level_3;  -- Exception raised here propagates up
      Put_Line(" This line never executes");
   end Level_2;

   procedure Level_1 is
   begin
      Put_Line("Entering Level_1");
      begin
         Level_2;
      exception
         when Calculation_Error =>
            Put_Line("Level_1 caught Calculation_Error");
            Put_Line("Handling the error...");
            -- Could re-raise with: raise;
      end;
      Put_Line("Level_1 continues after handling exception");
   end Level_1;

   -- Example with re-raise
   procedure Reraise_Example is
      procedure Inner is
      begin
         raise Constraint_Error;
      end Inner;
   begin
      Put_Line("Entering Reraise_Example");
      begin
         Inner;
      exception
         when Constraint_Error =>
            Put_Line("  Caught in Reraise_Example, re-raising...");
            raise;  -- Re-raise the same exception
      end;
   end Reraise_Example;

begin
   Put_Line("Exception propagation examples:");
   New_Line;

   -- Show propagation and handling
   Put_Line("=== Propagation Example ===");
   Level_1;
   New_Line;

   -- Show re-raise
   Put_Line("=== Re-raise Example ===");
   begin
      Reraise_Example;
   exception
      when Constraint_Error =>
         Put_Line("Caught re-raised Constraint_Error at top level");
   end;
   New_Line;

   Put_Line("Program completed normally");
end Exception_Propagation;
