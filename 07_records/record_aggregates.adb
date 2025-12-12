-- Example: record_aggregates.adb
-- Concept: Record aggregate syntax
--
-- Aggregates initialize records:
--   - Named: (Field1 => Value1, Field2 => Value2)
--   - Positional: (Value1, Value2)  -- in declaration order
--   - Others: (Field1 => Value1, others => DefaultValue)

with Ada.Text_IO;
use Ada.Text_IO;

procedure Record_Aggregates is
   type Date is record
      Year  : Integer;
      Month : Integer;
      Day   : Integer;
   end record;

   type Rectangle is record
      Left   : Integer := 0;
      Top    : Integer := 0;
      Width  : Integer := 100;
      Height : Integer := 100;
   end record;

   D1, D2, D3 : Date;
   R1, R2 : Rectangle;
begin
   Put_Line("Record aggregate examples:");

   -- Named aggregate (most readable)
   D1 := (Year => 2024, Month => 12, Day => 25);

   -- Positional aggregate (follows declaration order)
   D2 := (2024, 7, 4);

   -- Mixed (positional must come first)
   D3 := (2024, Month => 1, Day => 1);

   Put_Line("Date D1 (named): " &
            Integer'Image(D1.Year) & "-" &
            Integer'Image(D1.Month) & "-" &
            Integer'Image(D1.Day));

   Put_Line("Date D2 (positional): " &
            Integer'Image(D2.Year) & "-" &
            Integer'Image(D2.Month) & "-" &
            Integer'Image(D2.Day));

   Put_Line("Date D3 (mixed): " &
            Integer'Image(D3.Year) & "-" &
            Integer'Image(D3.Month) & "-" &
            Integer'Image(D3.Day));

   -- Using defaults
   R1 := (Left => 10, Top => 20, others => <>);  -- Width/Height get defaults
   Put_Line("Rectangle R1 (with defaults):");
   Put_Line("  Left=" & Integer'Image(R1.Left) &
            " Top=" & Integer'Image(R1.Top) &
            " Width=" & Integer'Image(R1.Width) &
            " Height=" & Integer'Image(R1.Height));

   -- Override some defaults
   R2 := (Width => 200, Height => 150, others => <>);
   Put_Line("Rectangle R2:");
   Put_Line("  Left=" & Integer'Image(R2.Left) &
            " Top=" & Integer'Image(R2.Top) &
            " Width=" & Integer'Image(R2.Width) &
            " Height=" & Integer'Image(R2.Height));

   -- Aggregate in expression
   if D1 = (Year => 2024, Month => 12, Day => 25) then
      Put_Line("D1 is Christmas 2024!");
   end if;
end Record_Aggregates;
