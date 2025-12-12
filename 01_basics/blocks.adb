-- Example: blocks.adb
-- Concept: Declare blocks for local scope
--
-- A "declare" block lets you introduce local declarations
-- in the middle of executable code. Variables declared
-- in a block only exist within that block.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Blocks is
   Outer : Integer := 100;
begin
   Put_Line("Outer =" & Integer'Image(Outer));

   -- A declare block creates a new scope
   declare
      Inner : Integer := 50;  -- Only visible in this block
      Local : Integer;
   begin
      Local := Outer + Inner;
      Put_Line("Inside block:");
      Put_Line("  Inner =" & Integer'Image(Inner));
      Put_Line("  Local =" & Integer'Image(Local));
      Put_Line("  Outer =" & Integer'Image(Outer));  -- Still visible
   end;

   -- Inner and Local no longer exist here
   Put_Line("After block, Outer =" & Integer'Image(Outer));

   -- Blocks can be named
   Named_Block:
   declare
      X : Integer := 42;
   begin
      Put_Line("In Named_Block, X =" & Integer'Image(X));
   end Named_Block;

end Blocks;
