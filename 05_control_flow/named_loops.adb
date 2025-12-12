-- Example: named_loops.adb
-- Concept: Named loops and targeted exit
--
-- Loops can be named for:
--   - Documentation/readability
--   - Exiting outer loops from nested loops
--   - exit Outer_Loop;  -- exits the named loop

with Ada.Text_IO;
use Ada.Text_IO;

procedure Named_Loops is
   Found : Boolean := False;
begin
   Put_Line("Named loop examples:");

   -- Named for loop
   Count_Loop:
   for I in 1 .. 3 loop
      Put_Line("  Iteration" & Integer'Image(I));
   end loop Count_Loop;  -- Name can be repeated at end

   -- Exit from nested loop
   Put_Line("Nested loops - finding a value:");

   Outer_Loop:
   for I in 1 .. 5 loop
      Inner_Loop:
      for J in 1 .. 5 loop
         Put("  Checking I=" & Integer'Image(I) &
             " J=" & Integer'Image(J));
         if I * J = 6 then
            Put_Line(" - FOUND!");
            Found := True;
            exit Outer_Loop;  -- Exit OUTER loop, not just inner
         end if;
         New_Line;
      end loop Inner_Loop;
   end loop Outer_Loop;

   if Found then
      Put_Line("Exited both loops when product = 6");
   end if;

   -- Named while loop
   Put_Line("Named while loop:");
   declare
      N : Integer := 0;
   begin
      Search:
      while N < 100 loop
         N := N + 7;  -- Add 7 each time
         if N > 50 then
            Put_Line("  Exiting at N =" & Integer'Image(N));
            exit Search;
         end if;
      end loop Search;
   end;

   -- Multiple exit points
   Put_Line("Multiple exits from named loop:");
   declare
      X : Integer := 0;
   begin
      Process:
      loop
         X := X + 1;
         if X > 10 then
            Put_Line("  Exit: X > 10");
            exit Process;
         end if;
         if X mod 7 = 0 then
            Put_Line("  Exit: X divisible by 7");
            exit Process;
         end if;
      end loop Process;
   end;
end Named_Loops;
