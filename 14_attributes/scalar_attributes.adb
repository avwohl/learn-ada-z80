-- Example: scalar_attributes.adb
-- Concept: Scalar type attributes
--
-- Attributes provide information about types and objects.
-- Format: Type'Attribute or Object'Attribute
-- Many attributes are evaluated at compile time.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Scalar_Attributes is
   type Small is range -100 .. 100;
   type Byte is mod 256;
   type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

   S : Small := 50;
   B : Byte := 200;
   D : Day := Wed;
begin
   Put_Line("Scalar type attributes:");

   -- Integer type attributes
   Put_Line("Integer attributes:");
   Put_Line("  Integer'First =" & Integer'Image(Integer'First));
   Put_Line("  Integer'Last =" & Integer'Image(Integer'Last));
   Put_Line("  Integer'Size =" & Integer'Image(Integer'Size) & " bits");

   -- Small range type
   Put_Line("Small (-100..100) attributes:");
   Put_Line("  Small'First =" & Small'Image(Small'First));
   Put_Line("  Small'Last =" & Small'Image(Small'Last));
   Put_Line("  Small'Range is " & Small'Image(Small'First) &
            " .." & Small'Image(Small'Last));

   -- Modular type
   Put_Line("Byte (mod 256) attributes:");
   Put_Line("  Byte'First =" & Byte'Image(Byte'First));
   Put_Line("  Byte'Last =" & Byte'Image(Byte'Last));
   Put_Line("  Byte'Modulus =" & Integer'Image(Byte'Modulus));

   -- Enumeration attributes
   Put_Line("Day enumeration attributes:");
   Put_Line("  Day'First = " & Day'Image(Day'First));
   Put_Line("  Day'Last = " & Day'Image(Day'Last));
   Put_Line("  Day'Pos(Wed) =" & Integer'Image(Day'Pos(Wed)));
   Put_Line("  Day'Val(0) = " & Day'Image(Day'Val(0)));
   Put_Line("  Day'Succ(Wed) = " & Day'Image(Day'Succ(Wed)));
   Put_Line("  Day'Pred(Wed) = " & Day'Image(Day'Pred(Wed)));

   -- Image and Value
   Put_Line("Image and Value:");
   Put_Line("  Integer'Image(42) = """ & Integer'Image(42) & """");
   Put_Line("  Day'Image(Fri) = """ & Day'Image(Fri) & """");
   -- Value converts string to type
   Put_Line("  Integer'Value(""123"") =" & Integer'Image(Integer'Value("123")));

   -- Min and Max
   Put_Line("Min and Max:");
   Put_Line("  Integer'Min(10, 20) =" & Integer'Image(Integer'Min(10, 20)));
   Put_Line("  Integer'Max(10, 20) =" & Integer'Image(Integer'Max(10, 20)));
end Scalar_Attributes;
