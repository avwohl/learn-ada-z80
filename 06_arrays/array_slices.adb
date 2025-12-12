-- Example: array_slices.adb
-- Concept: Array slices (extracting portions of arrays)
--
-- A slice is a contiguous portion of a one-dimensional array.
-- Syntax: Array_Name(Low .. High)
-- Slices can be read, assigned, and passed to subprograms.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Array_Slices is
   type Int_Array is array(1 .. 10) of Integer;

   A : Int_Array := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
   B : Int_Array := (others => 0);

   procedure Print_Slice(Name : String; S : Int_Array) is
   begin
      Put(Name & " =");
      for N of S loop
         Put(Integer'Image(N));
      end loop;
      New_Line;
   end Print_Slice;
begin
   Put_Line("Array slice examples:");

   -- Print original array
   Print_Slice("A", A);

   -- Read a slice
   Put_Line("Slice A(3..7):");
   Put("  ");
   for I in 3 .. 7 loop
      Put(Integer'Image(A(I)));
   end loop;
   New_Line;

   -- Assign to a slice
   B(1 .. 5) := A(6 .. 10);  -- Copy last 5 elements to first 5
   Put_Line("After B(1..5) := A(6..10):");
   Print_Slice("B", B);

   -- Slice of same array (overlapping slices need care)
   A(1 .. 5) := (100, 200, 300, 400, 500);
   Put_Line("After modifying A(1..5):");
   Print_Slice("A", A);

   -- String slices (strings are character arrays)
   declare
      S : String(1 .. 11) := "Hello World";
   begin
      Put_Line("String slicing:");
      Put_Line("  Full string: " & S);
      Put_Line("  S(1..5) = " & S(1 .. 5));
      Put_Line("  S(7..11) = " & S(7 .. 11));

      -- Modify string slice
      S(7 .. 11) := "Ada!!";
      Put_Line("  After S(7..11) := ""Ada!!"": " & S);
   end;

   -- Empty slice
   declare
      Empty : Int_Array := (others => 0);
   begin
      Empty(5 .. 4) := (others => <>);  -- Empty slice (5 > 4)
      Put_Line("Empty slice (5..4) is valid but contains nothing");
   end;
end Array_Slices;
