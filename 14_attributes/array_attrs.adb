-- Example: array_attrs.adb
-- Concept: Array attributes
--
-- Array attributes provide bounds and size information.
-- Dimension can be specified for multi-dimensional arrays.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Array_Attrs is
   type Vector is array(1 .. 10) of Integer;
   type Matrix is array(1 .. 3, 1 .. 4) of Integer;
   type Offset_Array is array(-5 .. 5) of Integer;

   V : Vector := (others => 0);
   M : Matrix := (others => (others => 0));
   O : Offset_Array := (others => 0);
begin
   Put_Line("Array attributes:");

   -- 1D array attributes
   Put_Line("Vector (1..10):");
   Put_Line("  V'First =" & Integer'Image(V'First));
   Put_Line("  V'Last =" & Integer'Image(V'Last));
   Put_Line("  V'Length =" & Integer'Image(V'Length));
   Put_Line("  V'Range is" & Integer'Image(V'First) &
            " .." & Integer'Image(V'Last));

   -- Offset array
   Put_Line("Offset_Array (-5..5):");
   Put_Line("  O'First =" & Integer'Image(O'First));
   Put_Line("  O'Last =" & Integer'Image(O'Last));
   Put_Line("  O'Length =" & Integer'Image(O'Length));

   -- 2D array attributes (specify dimension)
   Put_Line("Matrix (1..3, 1..4):");
   Put_Line("  M'First(1) =" & Integer'Image(M'First(1)) & " (rows)");
   Put_Line("  M'Last(1) =" & Integer'Image(M'Last(1)));
   Put_Line("  M'Length(1) =" & Integer'Image(M'Length(1)));
   Put_Line("  M'First(2) =" & Integer'Image(M'First(2)) & " (cols)");
   Put_Line("  M'Last(2) =" & Integer'Image(M'Last(2)));
   Put_Line("  M'Length(2) =" & Integer'Image(M'Length(2)));

   -- Using 'Range for safe iteration
   Put_Line("Safe iteration with 'Range:");
   Put("  V =");
   for I in V'Range loop
      V(I) := I * 10;
      Put(Integer'Image(V(I)));
   end loop;
   New_Line;

   -- Size attributes
   Put_Line("Size attributes:");
   Put_Line("  V'Size =" & Integer'Image(V'Size) & " bits");
   Put_Line("  V'Component_Size =" & Integer'Image(V'Component_Size) & " bits");

   -- Address attribute
   -- Put_Line("  V'Address = " & System.Address'Image(V'Address));
end Array_Attrs;
