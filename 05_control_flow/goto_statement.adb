-- Example: goto_statement.adb
-- Concept: Goto statement (use sparingly!)
--
-- Ada has goto for special cases, but structured control
-- flow (if, loop, exit) is usually better.
-- Labels are written: <<Label_Name>>

with Ada.Text_IO;
use Ada.Text_IO;

procedure Goto_Statement is
   N : Integer := 0;
begin
   Put_Line("Goto statement (use sparingly!):");

   -- Simple goto example
   Put_Line("Simple goto:");
   goto Skip_This;
   Put_Line("  This line is skipped");
   <<Skip_This>>
   Put_Line("  Jumped to here");

   -- Goto in a loop (exit is usually better)
   Put_Line("Loop with goto (exit is cleaner):");
   N := 0;
   <<Loop_Start>>
   N := N + 1;
   Put_Line("  N =" & Integer'Image(N));
   if N < 3 then
      goto Loop_Start;
   end if;

   -- Better version with loop and exit:
   Put_Line("Better: using exit instead of goto:");
   N := 0;
   loop
      N := N + 1;
      Put_Line("  N =" & Integer'Image(N));
      exit when N >= 3;
   end loop;

   -- Goto cannot jump into a block, loop, or if statement
   -- Goto cannot jump out of a subprogram
   -- These restrictions make goto safer in Ada than in C

   Put_Line("Prefer structured control flow over goto!");
   Put_Line("Use goto only when it genuinely simplifies code.");
end Goto_Statement;
