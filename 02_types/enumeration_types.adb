-- Example: enumeration_types.adb
-- Concept: User-defined enumeration types
--
-- Enumerations define a type with a fixed set of named values.
-- The compiler assigns ordinal positions starting from 0.
-- Enums are strongly typed - can't mix different enum types.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Enumeration_Types is
   -- Define enumeration types
   type Day is (Monday, Tuesday, Wednesday, Thursday,
                Friday, Saturday, Sunday);

   type Traffic_Light is (Red, Yellow, Green);

   type Direction is (North, East, South, West);

   Today     : Day := Wednesday;
   Light     : Traffic_Light := Red;
   Heading   : Direction := North;
begin
   Put_Line("Enumeration examples:");

   -- Use 'Image to convert to string
   Put_Line("  Today is " & Day'Image(Today));
   Put_Line("  Light is " & Traffic_Light'Image(Light));
   Put_Line("  Heading is " & Direction'Image(Heading));

   -- Ordinal position with 'Pos
   Put_Line("Ordinal positions (0-based):");
   Put_Line("  Monday'Pos =" & Integer'Image(Day'Pos(Monday)));
   Put_Line("  Wednesday'Pos =" & Integer'Image(Day'Pos(Wednesday)));
   Put_Line("  Sunday'Pos =" & Integer'Image(Day'Pos(Sunday)));

   -- Successor and Predecessor
   Put_Line("Navigation:");
   Put_Line("  Day after Wednesday: " & Day'Image(Day'Succ(Wednesday)));
   Put_Line("  Day before Wednesday: " & Day'Image(Day'Pred(Wednesday)));

   -- Iteration over enumeration
   Put_Line("All days:");
   for D in Day loop
      Put_Line("  " & Day'Image(D));
   end loop;

   -- Range attributes
   Put_Line("First day: " & Day'Image(Day'First));
   Put_Line("Last day: " & Day'Image(Day'Last));

   -- Case statement with enumeration
   case Light is
      when Red =>
         Put_Line("Stop!");
      when Yellow =>
         Put_Line("Caution!");
      when Green =>
         Put_Line("Go!");
   end case;
end Enumeration_Types;
