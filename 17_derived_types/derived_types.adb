-- Example: derived_types.adb
-- Concept: Derived types (type derivation)
--
-- Derived types create new types from existing ones.
-- They inherit operations but are distinct types.
-- Prevents accidental mixing of semantically different values.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Derived_Types is
   -- Derived from Integer
   type Meters is new Integer;
   type Feet is new Integer;
   type Kilograms is new Integer;

   -- Each is a distinct type!
   Distance_M : Meters := 100;
   Distance_F : Feet := 328;
   Weight     : Kilograms := 70;

   -- Derived type inherits operations
   type My_Int is new Integer;
   A, B, C : My_Int;

   -- Conversion factor
   Meters_Per_Foot : constant := 0;  -- Would be 0.3048 with float
begin
   Put_Line("Derived type examples:");

   -- Derived types prevent mixing
   Put_Line("Distinct types:");
   Put_Line("  Distance_M =" & Meters'Image(Distance_M) & " meters");
   Put_Line("  Distance_F =" & Feet'Image(Distance_F) & " feet");
   Put_Line("  Weight =" & Kilograms'Image(Weight) & " kg");

   -- Cannot assign without conversion:
   -- Distance_M := Distance_F;  -- Error! Different types
   -- Distance_M := Weight;       -- Error! Different types

   -- Must use explicit conversion
   Distance_M := Meters(Distance_F);  -- Explicit conversion
   Put_Line("After Distance_M := Meters(Distance_F):");
   Put_Line("  Distance_M =" & Meters'Image(Distance_M));

   -- Derived types inherit operations
   Put_Line("Inherited operations:");
   A := 10;
   B := 20;
   C := A + B;  -- + inherited from Integer
   Put_Line("  My_Int: A + B =" & My_Int'Image(C));

   C := A * B;  -- * inherited
   Put_Line("  My_Int: A * B =" & My_Int'Image(C));

   -- Comparison inherited
   if A < B then
      Put_Line("  My_Int: A < B is True");
   end if;

   -- Type safety in action
   Put_Line("Type safety:");
   declare
      procedure Print_Distance(M : Meters) is
      begin
         Put_Line("  Distance:" & Meters'Image(M) & " meters");
      end Print_Distance;
   begin
      Print_Distance(Distance_M);   -- OK: Meters
      -- Print_Distance(Distance_F);  -- Error! Feet not Meters
      -- Print_Distance(Weight);      -- Error! Kilograms not Meters
      Print_Distance(Meters(Distance_F));  -- OK with conversion
   end;
end Derived_Types;
