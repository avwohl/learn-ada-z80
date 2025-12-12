-- Example: basic_arrays.adb
-- Concept: Basic array declarations and access
--
-- Arrays in Ada are collections of elements of the same type.
-- Array indices can be any discrete type (integer, enum, char).
-- Arrays are bounds-checked at runtime.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Basic_Arrays is
   -- Define an array type and declare a variable
   type Int_Array is array(1 .. 5) of Integer;
   Numbers : Int_Array;

   -- Array with initialization
   Primes : Int_Array := (2, 3, 5, 7, 11);

   -- Anonymous array type (declared in variable)
   Scores : array(1 .. 3) of Integer := (85, 90, 78);
begin
   Put_Line("Basic array examples:");

   -- Initialize elements individually
   Numbers(1) := 10;
   Numbers(2) := 20;
   Numbers(3) := 30;
   Numbers(4) := 40;
   Numbers(5) := 50;

   -- Access elements
   Put_Line("Numbers array:");
   for I in 1 .. 5 loop
      Put_Line("  Numbers(" & Integer'Image(I) & ") =" &
               Integer'Image(Numbers(I)));
   end loop;

   -- Using an initialized array
   Put_Line("Primes array:");
   for I in 1 .. 5 loop
      Put_Line("  Primes(" & Integer'Image(I) & ") =" &
               Integer'Image(Primes(I)));
   end loop;

   -- Modify an element
   Numbers(3) := 300;
   Put_Line("After Numbers(3) := 300:");
   Put_Line("  Numbers(3) =" & Integer'Image(Numbers(3)));

   -- Array assignment (copies all elements)
   Numbers := Primes;
   Put_Line("After Numbers := Primes:");
   Put("  Numbers =");
   for N of Numbers loop
      Put(Integer'Image(N));
   end loop;
   New_Line;

   -- Array comparison
   if Numbers = Primes then
      Put_Line("Numbers equals Primes");
   end if;

   -- Out of bounds access raises Constraint_Error
   -- Numbers(6) := 100;  -- Would raise Constraint_Error
end Basic_Arrays;
