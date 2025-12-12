-- Example: array_of_arrays.adb
-- Concept: Arrays of arrays vs multidimensional arrays
--
-- Ada distinguishes between:
--   - Multidimensional array: array(1..M, 1..N) of T
--   - Array of arrays: array(1..M) of array(1..N) of T
-- The latter allows slicing of rows.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Array_Of_Arrays is
   -- Array of arrays (each row is a separate array)
   type Row is array(1 .. 4) of Integer;
   type Table is array(1 .. 3) of Row;

   T : Table;

   -- Compare with true 2D array
   type Matrix is array(1 .. 3, 1 .. 4) of Integer;
   M : Matrix;
begin
   Put_Line("Array of arrays examples:");

   -- Initialize array of arrays
   T(1) := (1, 2, 3, 4);
   T(2) := (5, 6, 7, 8);
   T(3) := (9, 10, 11, 12);

   -- Access: Table(row)(col)
   Put_Line("Array of arrays T:");
   for R in T'Range loop
      Put("  Row" & Integer'Image(R) & ":");
      for C in T(R)'Range loop
         Put(Integer'Image(T(R)(C)));
      end loop;
      New_Line;
   end loop;

   -- Can treat whole row as array
   Put_Line("Accessing row as array:");
   declare
      Row2 : Row := T(2);  -- Copy entire row
   begin
      Put("  T(2) as Row:");
      for N of Row2 loop
         Put(Integer'Image(N));
      end loop;
      New_Line;
   end;

   -- Can assign entire row
   T(3) := T(1);  -- Copy row 1 to row 3
   Put_Line("After T(3) := T(1):");
   Put("  T(3) =");
   for N of T(3) loop
      Put(Integer'Image(N));
   end loop;
   New_Line;

   -- Initialize true 2D array for comparison
   M := ((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12));

   Put_Line("True 2D matrix M (accessed with M(row, col)):");
   for R in M'Range(1) loop
      Put("  Row" & Integer'Image(R) & ":");
      for C in M'Range(2) loop
         Put(Integer'Image(M(R, C)));
      end loop;
      New_Line;
   end loop;

   -- Note: Can't do M(2) to get a whole row from true 2D array
   -- That's the key difference!
end Array_Of_Arrays;
