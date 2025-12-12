-- Example: named_parameters.adb
-- Concept: Named parameter association
--
-- Parameters can be specified by name, not just position.
-- Improves readability and allows any order.
-- Can mix positional and named (positional first).

with Ada.Text_IO;
use Ada.Text_IO;

procedure Named_Parameters is

   procedure Draw_Rectangle(X      : Integer;
                            Y      : Integer;
                            Width  : Integer;
                            Height : Integer;
                            Filled : Boolean := False) is
   begin
      Put_Line("Rectangle at (" & Integer'Image(X) & "," &
               Integer'Image(Y) & ")");
      Put_Line("  Width:" & Integer'Image(Width) &
               " Height:" & Integer'Image(Height));
      Put_Line("  Filled: " & Boolean'Image(Filled));
   end Draw_Rectangle;

   function Create_Date(Year  : Integer;
                        Month : Integer;
                        Day   : Integer) return String is
   begin
      return Integer'Image(Year) & "-" &
             Integer'Image(Month) & "-" &
             Integer'Image(Day);
   end Create_Date;

begin
   Put_Line("Named parameter examples:");
   New_Line;

   -- Positional (traditional)
   Put_Line("1. Positional parameters:");
   Draw_Rectangle(10, 20, 100, 50, True);
   New_Line;

   -- Named (any order!)
   Put_Line("2. Named parameters (different order):");
   Draw_Rectangle(Width  => 100,
                  Height => 50,
                  X      => 10,
                  Y      => 20,
                  Filled => True);
   New_Line;

   -- Mixed (positional first, then named)
   Put_Line("3. Mixed (positional then named):");
   Draw_Rectangle(10, 20, Width => 100, Height => 50);
   New_Line;

   -- Named makes intent clearer
   Put_Line("4. Named for clarity:");
   Draw_Rectangle(X => 0, Y => 0, Width => 640, Height => 480);
   New_Line;

   -- Skip optional parameters using named
   Put_Line("5. With default (Filled omitted):");
   Draw_Rectangle(X => 50, Y => 50, Width => 200, Height => 100);
   New_Line;

   -- Date example - named prevents month/day confusion
   Put_Line("6. Date creation:");
   Put_Line("  Positional (2024, 12, 25): " & Create_Date(2024, 12, 25));
   Put_Line("  Named: " & Create_Date(Day => 25, Month => 12, Year => 2024));
   Put_Line("  (Named prevents month/day confusion!)");
end Named_Parameters;
