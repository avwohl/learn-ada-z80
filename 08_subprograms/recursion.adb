-- Example: recursion.adb
-- Concept: Recursive subprograms
--
-- A subprogram can call itself.
-- Need a base case to stop recursion.
-- Useful for naturally recursive problems.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Recursion is

   -- Classic factorial using recursion
   function Factorial(N : Natural) return Natural is
   begin
      if N <= 1 then
         return 1;  -- Base case
      else
         return N * Factorial(N - 1);  -- Recursive case
      end if;
   end Factorial;

   -- Recursive sum of array
   type Int_Array is array(Positive range <>) of Integer;

   function Sum(A : Int_Array) return Integer is
   begin
      if A'Length = 0 then
         return 0;  -- Base case: empty array
      elsif A'Length = 1 then
         return A(A'First);  -- Base case: single element
      else
         return A(A'First) + Sum(A(A'First + 1 .. A'Last));
      end if;
   end Sum;

   -- Greatest common divisor (Euclidean algorithm)
   function GCD(A, B : Positive) return Positive is
   begin
      if B = 1 then
         return 1;
      elsif A mod B = 0 then
         return B;  -- Base case
      else
         return GCD(B, A mod B);  -- Recursive case
      end if;
   end GCD;

   -- Binary search (recursive)
   function Binary_Search(A     : Int_Array;
                          Value : Integer;
                          Low   : Integer;
                          High  : Integer) return Integer is
      Mid : Integer;
   begin
      if Low > High then
         return -1;  -- Not found
      end if;

      Mid := (Low + High) / 2;

      if A(Mid) = Value then
         return Mid;  -- Found
      elsif A(Mid) > Value then
         return Binary_Search(A, Value, Low, Mid - 1);
      else
         return Binary_Search(A, Value, Mid + 1, High);
      end if;
   end Binary_Search;

   -- Recursive procedure (countdown)
   procedure Countdown(N : Natural) is
   begin
      if N = 0 then
         Put_Line("  Blastoff!");
      else
         Put_Line("  " & Natural'Image(N));
         Countdown(N - 1);
      end if;
   end Countdown;

   Numbers : Int_Array := (1, 3, 5, 7, 9, 11, 13, 15);
   Idx : Integer;
begin
   Put_Line("Recursion examples:");

   -- Factorial
   Put_Line("Factorial:");
   for I in 0 .. 7 loop
      Put_Line("  " & Natural'Image(I) & "! =" &
               Natural'Image(Factorial(I)));
   end loop;

   -- Sum of array
   Put_Line("Sum of (1,3,5,7,9,11,13,15) =" &
            Integer'Image(Sum(Numbers)));

   -- GCD
   Put_Line("GCD:");
   Put_Line("  GCD(48, 18) =" & Positive'Image(GCD(48, 18)));
   Put_Line("  GCD(100, 35) =" & Positive'Image(GCD(100, 35)));

   -- Binary search
   Put_Line("Binary search in sorted array:");
   Idx := Binary_Search(Numbers, 7, Numbers'First, Numbers'Last);
   Put_Line("  Found 7 at index" & Integer'Image(Idx));

   Idx := Binary_Search(Numbers, 6, Numbers'First, Numbers'Last);
   Put_Line("  Found 6 at index" & Integer'Image(Idx) & " (not found)");

   -- Countdown
   Put_Line("Countdown from 5:");
   Countdown(5);
end Recursion;
