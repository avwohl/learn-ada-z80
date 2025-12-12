-- Example: unconstrained_arrays.adb
-- Concept: Unconstrained array types
--
-- Unconstrained arrays have bounds determined at runtime.
-- The type declaration specifies index type but not bounds.
-- Bounds are set when variable is declared or parameter passed.
-- String is the most common unconstrained array type.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Unconstrained_Arrays is
   -- Unconstrained array type (bounds not specified)
   type Int_Vector is array(Integer range <>) of Integer;
   type Char_Vector is array(Positive range <>) of Character;

   -- Variables must specify bounds
   Small : Int_Vector(1 .. 3) := (10, 20, 30);
   Large : Int_Vector(1 .. 10) := (others => 0);

   -- Different starting index
   Offset : Int_Vector(5 .. 8) := (100, 200, 300, 400);

   -- Procedure that takes any Int_Vector
   procedure Print_Vector(Name : String; V : Int_Vector) is
   begin
      Put(Name & " (" & Integer'Image(V'First) & ".." &
          Integer'Image(V'Last) & ") =");
      for N of V loop
         Put(Integer'Image(N));
      end loop;
      New_Line;
   end Print_Vector;

   -- Function that returns an array
   function Make_Vector(Size : Positive) return Int_Vector is
      Result : Int_Vector(1 .. Size);
   begin
      for I in Result'Range loop
         Result(I) := I * I;
      end loop;
      return Result;
   end Make_Vector;
begin
   Put_Line("Unconstrained array examples:");

   Print_Vector("Small", Small);
   Print_Vector("Large", Large);
   Print_Vector("Offset", Offset);

   -- String is unconstrained: type String is array(Positive range <>) of Character
   declare
      S1 : String(1 .. 5) := "Hello";
      S2 : String(1 .. 3) := "Ada";
   begin
      Put_Line("String (unconstrained):");
      Put_Line("  S1 = " & S1 & " Length:" & Integer'Image(S1'Length));
      Put_Line("  S2 = " & S2 & " Length:" & Integer'Image(S2'Length));
   end;

   -- Dynamic array from function
   declare
      V : Int_Vector := Make_Vector(5);
   begin
      Print_Vector("Make_Vector(5)", V);
   end;

   -- Array comparison works with different bounds
   -- (compares element by element, sliding to match)
   declare
      A : Int_Vector(1 .. 3) := (1, 2, 3);
      B : Int_Vector(5 .. 7) := (1, 2, 3);
   begin
      Put_Line("Arrays with different bounds but same values:");
      Put_Line("  A = B is " & Boolean'Image(A = B));
   end;
end Unconstrained_Arrays;
