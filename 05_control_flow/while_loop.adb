-- Example: while_loop.adb
-- Concept: While loops
--
-- While loop syntax:
--   while condition loop
--      statements;
--   end loop;
-- The condition is checked before each iteration.

with Ada.Text_IO;
use Ada.Text_IO;

procedure While_Loop is
   Count : Integer;
   Sum   : Integer;
   N     : Integer;
begin
   Put_Line("While loop examples:");

   -- Basic while loop
   Put_Line("Counting down from 5:");
   Count := 5;
   while Count > 0 loop
      Put_Line("  " & Integer'Image(Count));
      Count := Count - 1;
   end loop;

   -- Sum numbers 1 to 10
   Sum := 0;
   N := 1;
   while N <= 10 loop
      Sum := Sum + N;
      N := N + 1;
   end loop;
   Put_Line("Sum of 1 to 10 =" & Integer'Image(Sum));

   -- Loop that might not execute at all
   Count := 0;
   while Count > 0 loop
      Put_Line("This won't print");
      Count := Count - 1;
   end loop;
   Put_Line("Loop with false condition never executed");

   -- Finding a value
   N := 1;
   while N * N < 50 loop
      N := N + 1;
   end loop;
   Put_Line("First N where N*N >= 50: N =" & Integer'Image(N));

   -- Named while loop (for exit statements)
   Search_Loop:
   while True loop
      Count := Count + 1;
      if Count > 10 then
         exit Search_Loop;  -- Exit the named loop
      end if;
   end loop Search_Loop;
   Put_Line("Exited named loop at count" & Integer'Image(Count));
end While_Loop;
