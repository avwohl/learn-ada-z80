-- Example: program_structure.adb
-- Concept: Basic structure of an Ada program
--
-- An Ada program consists of:
--   1. Context clauses (with/use) - imports
--   2. The main subprogram (procedure)
--   3. Declarative part (between "is" and "begin")
--   4. Executable part (between "begin" and "end")
--   5. Exception handlers (optional, before "end")

with Ada.Text_IO;    -- Context clause: import Text_IO
use Ada.Text_IO;     -- Context clause: make names visible

procedure Program_Structure is
   --==========================================
   -- DECLARATIVE PART
   -- Variables, constants, types go here
   --==========================================

   Program_Name : constant String := "Structure Demo";
   Counter      : Integer := 0;

   -- Local procedure declaration
   procedure Print_Separator is
   begin
      Put_Line("------------------------");
   end Print_Separator;

begin
   --==========================================
   -- EXECUTABLE PART
   -- Statements go here
   --==========================================

   Print_Separator;
   Put_Line("Program: " & Program_Name);
   Print_Separator;

   Counter := Counter + 1;
   Put_Line("Counter =" & Integer'Image(Counter));

   Print_Separator;
   Put_Line("Program completed successfully.");

exception
   --==========================================
   -- EXCEPTION HANDLER PART (optional)
   --==========================================
   when others =>
      Put_Line("An error occurred!");

end Program_Structure;
