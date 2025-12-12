-- Example: array_aggregates.adb
-- Concept: Array aggregates (initialization syntax)
--
-- Aggregates provide flexible ways to initialize arrays:
--   - Positional: (1, 2, 3)
--   - Named: (1 => 10, 2 => 20, 3 => 30)
--   - Others: (others => 0)
--   - Mixed: (1 => 100, others => 0)

with Ada.Text_IO;
use Ada.Text_IO;

procedure Array_Aggregates is
   type Arr5 is array(1 .. 5) of Integer;
   type Arr10 is array(1 .. 10) of Integer;

   -- Positional aggregate - values in order
   A : Arr5 := (10, 20, 30, 40, 50);

   -- Named aggregate - explicit index assignment
   B : Arr5 := (1 => 100, 2 => 200, 3 => 300, 4 => 400, 5 => 500);

   -- Others clause - default for remaining elements
   C : Arr10 := (1 => 1, 2 => 2, others => 0);

   -- Range in aggregate
   D : Arr10 := (1 .. 3 => 10, 4 .. 6 => 20, 7 .. 10 => 30);

   -- All same value
   E : Arr5 := (others => 42);

   -- Mixed positional and named (positional must come first)
   F : Arr5 := (100, 200, 3 => 300, 4 => 400, 5 => 500);

   procedure Print_Array(Name : String; Arr : Arr5) is
   begin
      Put(Name & " =");
      for N of Arr loop
         Put(Integer'Image(N));
      end loop;
      New_Line;
   end Print_Array;
begin
   Put_Line("Array aggregate examples:");

   Print_Array("A (positional)", A);
   Print_Array("B (named)", B);

   Put("C (1=>1, 2=>2, others=>0) =");
   for N of C loop
      Put(Integer'Image(N));
   end loop;
   New_Line;

   Put("D (ranges) =");
   for N of D loop
      Put(Integer'Image(N));
   end loop;
   New_Line;

   Print_Array("E (others => 42)", E);
   Print_Array("F (mixed)", F);

   -- Aggregate in assignment
   A := (5, 4, 3, 2, 1);
   Print_Array("A reversed", A);

   -- Using | for multiple indices
   declare
      G : Arr5 := (1 | 3 | 5 => 100, 2 | 4 => 0);
   begin
      Print_Array("G (alternating)", G);
   end;
end Array_Aggregates;
