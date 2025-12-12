-- Example: array_attributes.adb
-- Concept: Array attributes
--
-- Key array attributes:
--   'First  - Lower bound of array index
--   'Last   - Upper bound of array index
--   'Range  - Equivalent to 'First .. 'Last
--   'Length - Number of elements

with Ada.Text_IO;
use Ada.Text_IO;

procedure Array_Attributes is
   type Arr_1_5 is array(1 .. 5) of Integer;
   type Arr_0_9 is array(0 .. 9) of Integer;

   A : Arr_1_5 := (10, 20, 30, 40, 50);
   B : Arr_0_9 := (others => 0);

   -- Array with non-standard bounds
   type Arr_Neg is array(-5 .. 5) of Integer;
   C : Arr_Neg := (others => 1);

   -- Character-indexed array
   type Char_Arr is array('A' .. 'Z') of Integer;
   Letter_Counts : Char_Arr := (others => 0);
begin
   Put_Line("Array attribute examples:");

   -- 'First, 'Last, 'Length
   Put_Line("Array A (1..5):");
   Put_Line("  A'First =" & Integer'Image(A'First));
   Put_Line("  A'Last =" & Integer'Image(A'Last));
   Put_Line("  A'Length =" & Integer'Image(A'Length));

   Put_Line("Array B (0..9):");
   Put_Line("  B'First =" & Integer'Image(B'First));
   Put_Line("  B'Last =" & Integer'Image(B'Last));
   Put_Line("  B'Length =" & Integer'Image(B'Length));

   Put_Line("Array C (-5..5):");
   Put_Line("  C'First =" & Integer'Image(C'First));
   Put_Line("  C'Last =" & Integer'Image(C'Last));
   Put_Line("  C'Length =" & Integer'Image(C'Length));

   -- Using 'Range in for loop (most common idiom)
   Put_Line("Iterating with 'Range:");
   Put("  A =");
   for I in A'Range loop
      Put(Integer'Image(A(I)));
   end loop;
   New_Line;

   -- Character-indexed array bounds
   Put_Line("Character array (A..Z):");
   Put_Line("  Letter_Counts'First = " & Character'Image(Letter_Counts'First));
   Put_Line("  Letter_Counts'Last = " & Character'Image(Letter_Counts'Last));
   Put_Line("  Letter_Counts'Length =" & Integer'Image(Letter_Counts'Length));

   -- Practical use: safe iteration regardless of bounds
   Put_Line("Safe iteration pattern:");
   for I in B'Range loop
      B(I) := I * 10;
   end loop;
   Put("  B =");
   for I in B'Range loop
      Put(Integer'Image(B(I)));
   end loop;
   New_Line;
end Array_Attributes;
