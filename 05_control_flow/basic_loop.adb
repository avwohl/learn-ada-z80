-- Example: basic_loop.adb
-- Concept: Basic (infinite) loops with exit
--
-- Basic loop syntax:
--   loop
--      statements;
--      exit when condition;  -- or just: exit;
--   end loop;
-- Must have an exit statement to terminate!

with Ada.Text_IO;
use Ada.Text_IO;

procedure Basic_Loop is
   Count : Integer := 0;
   Sum   : Integer := 0;
   N     : Integer := 1;
begin
   Put_Line("Basic loop examples:");

   -- Loop with exit when
   Put_Line("Count to 5:");
   Count := 0;
   loop
      Count := Count + 1;
      Put_Line("  Count =" & Integer'Image(Count));
      exit when Count >= 5;
   end loop;

   -- Exit at beginning (like while)
   Put_Line("Exit at top (like while):");
   N := 3;
   loop
      exit when N <= 0;
      Put_Line("  N =" & Integer'Image(N));
      N := N - 1;
   end loop;

   -- Exit in middle
   Put_Line("Exit in middle:");
   N := 0;
   loop
      N := N + 1;
      if N > 3 then
         exit;
      end if;
      Put_Line("  Processing N =" & Integer'Image(N));
   end loop;

   -- Unconditional exit (break out immediately)
   Put_Line("Unconditional exit:");
   loop
      Put_Line("  This prints once");
      exit;  -- Exit immediately
      Put_Line("  This never prints");
   end loop;

   -- Loop until finding a condition
   Put_Line("Find first power of 2 >= 100:");
   N := 1;
   loop
      exit when N >= 100;
      N := N * 2;
   end loop;
   Put_Line("  Found N =" & Integer'Image(N));
end Basic_Loop;
