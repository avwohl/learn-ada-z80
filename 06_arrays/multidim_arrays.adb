-- Example: multidim_arrays.adb
-- Concept: Multi-dimensional arrays (matrices)
--
-- Ada supports true multi-dimensional arrays.
-- Each dimension can have different bounds and types.
-- Access: Array(Row, Column) or Array(I, J, K)

with Ada.Text_IO;
use Ada.Text_IO;

procedure Multidim_Arrays is
   -- 2D array (3 rows, 4 columns)
   type Matrix_3x4 is array(1 .. 3, 1 .. 4) of Integer;

   -- 3D array
   type Cube_2x2x2 is array(1 .. 2, 1 .. 2, 1 .. 2) of Integer;

   -- Chess board (character indexed)
   type Chess_Board is array('A' .. 'H', 1 .. 8) of Character;

   M : Matrix_3x4;
   C : Cube_2x2x2;
begin
   Put_Line("Multi-dimensional array examples:");

   -- Initialize 2D array
   for Row in 1 .. 3 loop
      for Col in 1 .. 4 loop
         M(Row, Col) := Row * 10 + Col;
      end loop;
   end loop;

   -- Print matrix
   Put_Line("Matrix M (Row*10 + Col):");
   for Row in 1 .. 3 loop
      Put("  Row" & Integer'Image(Row) & ":");
      for Col in 1 .. 4 loop
         Put(Integer'Image(M(Row, Col)));
      end loop;
      New_Line;
   end loop;

   -- Access individual element
   Put_Line("M(2, 3) =" & Integer'Image(M(2, 3)));

   -- 2D array attributes
   Put_Line("Matrix attributes:");
   Put_Line("  M'First(1) =" & Integer'Image(M'First(1)) & " (first row)");
   Put_Line("  M'Last(1) =" & Integer'Image(M'Last(1)) & " (last row)");
   Put_Line("  M'First(2) =" & Integer'Image(M'First(2)) & " (first col)");
   Put_Line("  M'Last(2) =" & Integer'Image(M'Last(2)) & " (last col)");
   Put_Line("  M'Length(1) =" & Integer'Image(M'Length(1)) & " (rows)");
   Put_Line("  M'Length(2) =" & Integer'Image(M'Length(2)) & " (cols)");

   -- 3D array initialization
   C := (1 => ((1, 2), (3, 4)),
         2 => ((5, 6), (7, 8)));

   Put_Line("3D array C:");
   for I in 1 .. 2 loop
      for J in 1 .. 2 loop
         for K in 1 .. 2 loop
            Put_Line("  C(" & Integer'Image(I) & "," &
                     Integer'Image(J) & "," & Integer'Image(K) &
                     ") =" & Integer'Image(C(I, J, K)));
         end loop;
      end loop;
   end loop;

   -- Aggregate for 2D array
   M := ((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12));
   Put_Line("Matrix from aggregate:");
   for Row in M'Range(1) loop
      Put("  ");
      for Col in M'Range(2) loop
         Put(Integer'Image(M(Row, Col)));
      end loop;
      New_Line;
   end loop;
end Multidim_Arrays;
