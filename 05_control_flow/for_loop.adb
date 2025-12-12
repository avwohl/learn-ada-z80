-- Example: for_loop.adb
-- Concept: For loops with ranges
--
-- For loop syntax:
--   for Variable in Range loop
--      statements;
--   end loop;
-- The loop variable is read-only and local to the loop.

with Ada.Text_IO;
use Ada.Text_IO;

procedure For_Loop is
   Sum : Integer := 0;

   type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
begin
   Put_Line("For loop examples:");

   -- Basic for loop with integer range
   Put_Line("Counting 1 to 5:");
   for I in 1 .. 5 loop
      Put_Line("  I =" & Integer'Image(I));
   end loop;

   -- Reverse iteration
   Put_Line("Counting down 5 to 1:");
   for I in reverse 1 .. 5 loop
      Put_Line("  I =" & Integer'Image(I));
   end loop;

   -- Summing with for loop
   Sum := 0;
   for N in 1 .. 100 loop
      Sum := Sum + N;
   end loop;
   Put_Line("Sum of 1 to 100 =" & Integer'Image(Sum));

   -- Loop over enumeration type
   Put_Line("Days of the week:");
   for D in Day loop
      Put_Line("  " & Day'Image(D));
   end loop;

   -- Loop over subrange of enumeration
   Put_Line("Weekdays only:");
   for D in Mon .. Fri loop
      Put_Line("  " & Day'Image(D));
   end loop;

   -- Loop over character range
   Put_Line("Characters A to E:");
   for C in 'A' .. 'E' loop
      Put("  ");
      Ada.Text_IO.Put(C);
      New_Line;
   end loop;

   -- The loop variable is read-only
   -- for I in 1 .. 5 loop
   --    I := I + 1;  -- Error! Cannot assign to loop variable
   -- end loop;

   -- Empty range means loop doesn't execute
   Put_Line("Loop with empty range:");
   for I in 5 .. 1 loop   -- Empty range (5 > 1)
      Put_Line("This won't print");
   end loop;
   Put_Line("  (nothing printed)");
end For_Loop;
