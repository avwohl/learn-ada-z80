-- Example: for_of_loop.adb
-- Concept: Ada 2012 "for ... of" iterator loops
--
-- The "for Element of Container" syntax iterates over
-- elements directly, rather than indices.
-- Simpler than "for I in Array'Range" when you don't need index.

with Ada.Text_IO;
use Ada.Text_IO;

procedure For_Of_Loop is
   type Int_Array is array(1..5) of Integer;
   Numbers : Int_Array := (10, 20, 30, 40, 50);

   Sum : Integer := 0;

   -- String is also an array
   Message : constant String := "HELLO";
begin
   Put_Line("For-of loop examples (Ada 2012):");

   -- Traditional index-based loop
   Put_Line("Traditional for loop with indices:");
   for I in Numbers'Range loop
      Put_Line("  Numbers(" & Integer'Image(I) & ") =" &
               Integer'Image(Numbers(I)));
   end loop;

   -- For-of loop - iterate over elements directly
   Put_Line("For-of loop over elements:");
   for N of Numbers loop
      Put_Line("  Element =" & Integer'Image(N));
   end loop;

   -- Sum using for-of
   Sum := 0;
   for N of Numbers loop
      Sum := Sum + N;
   end loop;
   Put_Line("Sum of elements =" & Integer'Image(Sum));

   -- For-of with characters in string
   Put_Line("For-of loop over string:");
   Put("  Characters in HELLO: ");
   for C of Message loop
      Put(C);
      Put(' ');
   end loop;
   New_Line;

   -- Modifying array elements with for-of
   Put_Line("Modifying elements (doubling):");
   declare
      Mutable : Int_Array := (1, 2, 3, 4, 5);
   begin
      -- Note: Element variable is a reference
      for N of Mutable loop
         N := N * 2;  -- Modifies the actual array element
      end loop;

      Put("  After doubling:");
      for N of Mutable loop
         Put(Integer'Image(N));
      end loop;
      New_Line;
   end;
end For_Of_Loop;
