-- Example: range_types.adb
-- Concept: Defining custom integer ranges
--
-- Ada lets you define integer types with specific ranges.
-- The compiler can use this for:
--   - Optimizing storage (smaller types)
--   - Range checking at runtime
--   - Documentation of valid values

with Ada.Text_IO;
use Ada.Text_IO;

procedure Range_Types is
   -- Custom range types
   type Byte is range 0 .. 255;           -- 8-bit unsigned
   type Signed_Byte is range -128 .. 127; -- 8-bit signed
   type Percentage is range 0 .. 100;     -- Logical constraint
   type Day_Of_Month is range 1 .. 31;    -- Calendar constraint
   type Temperature is range -40 .. 125;  -- Sensor range

   B : Byte := 200;
   P : Percentage := 75;
   D : Day_Of_Month := 15;
   T : Temperature := 22;
begin
   Put_Line("Custom range types:");
   Put_Line("  Byte (0..255): " & Byte'Image(B));
   Put_Line("  Percentage (0..100): " & Percentage'Image(P));
   Put_Line("  Day_Of_Month (1..31): " & Day_Of_Month'Image(D));
   Put_Line("  Temperature (-40..125): " & Temperature'Image(T));

   -- Show range bounds using attributes
   Put_Line("Percentage range:" &
            Percentage'Image(Percentage'First) & " .." &
            Percentage'Image(Percentage'Last));

   -- This would raise Constraint_Error at runtime:
   -- P := 150;  -- Out of range!
end Range_Types;
