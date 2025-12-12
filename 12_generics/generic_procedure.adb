-- Example: generic_procedure.adb
-- Concept: Generic procedures
--
-- Generics provide parametric polymorphism.
-- Write once, use with many types.
-- Must instantiate generic before use.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Generic_Procedure is

   -- Generic swap procedure
   generic
      type Element_Type is private;  -- Works with any type
   procedure Generic_Swap(Left, Right : in Out Element_Type);

   procedure Generic_Swap(Left, Right : in Out Element_Type) is
      Temp : Element_Type;
   begin
      Temp := Left;
      Left := Right;
      Right := Temp;
   end Generic_Swap;

   -- Instantiate for specific types
   procedure Swap_Int is new Generic_Swap(Integer);
   procedure Swap_Char is new Generic_Swap(Character);
   procedure Swap_Bool is new Generic_Swap(Boolean);

   -- Generic procedure with discrete type
   generic
      type Index_Type is (<>);  -- Any discrete type
   procedure Print_Range;

   procedure Print_Range is
   begin
      Put("Range: ");
      for I in Index_Type loop
         Put(Index_Type'Image(I) & " ");
      end loop;
      New_Line;
   end Print_Range;

   type Day is (Mon, Tue, Wed, Thu, Fri);
   procedure Print_Days is new Print_Range(Day);
   procedure Print_Chars is new Print_Range(Character range 'A' .. 'E');

   A, B : Integer;
   C, D : Character;
   E, F : Boolean;
begin
   Put_Line("Generic procedure examples:");

   -- Use Swap_Int
   A := 10;
   B := 20;
   Put_Line("Before swap: A=" & Integer'Image(A) &
            " B=" & Integer'Image(B));
   Swap_Int(A, B);
   Put_Line("After swap:  A=" & Integer'Image(A) &
            " B=" & Integer'Image(B));

   -- Use Swap_Char
   C := 'X';
   D := 'Y';
   Put_Line("Before swap: C=" & C & " D=" & D);
   Swap_Char(C, D);
   Put_Line("After swap:  C=" & C & " D=" & D);

   -- Use Swap_Bool
   E := True;
   F := False;
   Put_Line("Before swap: E=" & Boolean'Image(E) &
            " F=" & Boolean'Image(F));
   Swap_Bool(E, F);
   Put_Line("After swap:  E=" & Boolean'Image(E) &
            " F=" & Boolean'Image(F));

   -- Print ranges
   Put_Line("Day range:");
   Print_Days;

   Put_Line("Character A..E range:");
   Print_Chars;
end Generic_Procedure;
