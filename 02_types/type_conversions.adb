-- Example: type_conversions.adb
-- Concept: Type conversions in Ada
--
-- Ada is strongly typed - you must explicitly convert between types.
-- Conversion syntax: Target_Type(value)
-- This prevents accidental mixing of incompatible values.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Type_Conversions is
   type Meters is new Integer;
   type Feet is new Integer;

   M : Meters := 100;
   F : Feet := 328;
   I : Integer := 42;

   type Byte is mod 256;
   B : Byte := 200;

   C : Character;
   N : Integer;
begin
   Put_Line("Type conversion examples:");

   -- Cannot directly assign between derived types:
   -- M := F;  -- Error! Different types
   -- M := I;  -- Error! Different types

   -- Must use explicit conversion
   M := Meters(F);      -- Convert Feet to Meters
   F := Feet(M);        -- Convert Meters to Feet
   I := Integer(M);     -- Convert Meters to Integer
   M := Meters(I);      -- Convert Integer to Meters

   Put_Line("  Meters M =" & Meters'Image(M));
   Put_Line("  Feet F =" & Feet'Image(F));
   Put_Line("  Integer I =" & Integer'Image(I));

   -- Converting between Integer and modular
   I := Integer(B);     -- Modular to Integer
   B := Byte(I mod 256); -- Integer to modular (may need mod)
   Put_Line("  Byte B =" & Byte'Image(B));

   -- Character position conversion
   N := Character'Pos('A');  -- Get ordinal value
   C := Character'Val(N);     -- Get character from ordinal
   Put_Line("  Character 'A' has position" & Integer'Image(N));

   -- Range checking during conversion
   declare
      subtype Small is Integer range 0 .. 10;
      S : Small;
   begin
      S := Small(5);  -- OK
      Put_Line("  Small S =" & Small'Image(S));
      -- S := Small(100);  -- Would raise Constraint_Error
   end;

   Put_Line("Explicit conversion prevents type mixing errors.");
end Type_Conversions;
