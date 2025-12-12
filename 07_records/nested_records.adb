-- Example: nested_records.adb
-- Concept: Records containing other records
--
-- Records can contain other record types as fields.
-- Access nested fields with chained dot notation.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Nested_Records is
   type Point is record
      X : Integer;
      Y : Integer;
   end record;

   type Rectangle is record
      Top_Left     : Point;
      Bottom_Right : Point;
   end record;

   type Address is record
      Street  : String(1 .. 30);
      City    : String(1 .. 20);
      Zipcode : String(1 .. 10);
   end record;

   type Employee is record
      Name    : String(1 .. 20);
      Home    : Address;
      Office  : Address;
   end record;

   R : Rectangle;
   E : Employee;
begin
   Put_Line("Nested record examples:");

   -- Initialize nested record with aggregates
   R := (Top_Left     => (X => 0, Y => 0),
         Bottom_Right => (X => 100, Y => 50));

   -- Access nested fields
   Put_Line("Rectangle R:");
   Put_Line("  Top_Left: (" & Integer'Image(R.Top_Left.X) &
            "," & Integer'Image(R.Top_Left.Y) & ")");
   Put_Line("  Bottom_Right: (" & Integer'Image(R.Bottom_Right.X) &
            "," & Integer'Image(R.Bottom_Right.Y) & ")");

   -- Modify nested field
   R.Top_Left.X := 10;
   R.Top_Left.Y := 10;
   Put_Line("After modifying Top_Left:");
   Put_Line("  Top_Left: (" & Integer'Image(R.Top_Left.X) &
            "," & Integer'Image(R.Top_Left.Y) & ")");

   -- Assign to entire nested record
   R.Bottom_Right := (X => 200, Y => 100);

   -- Initialize deeply nested record
   E := (Name   => "John Smith          ",
         Home   => (Street  => "123 Main Street           ",
                    City    => "Springfield         ",
                    Zipcode => "12345     "),
         Office => (Street  => "456 Business Ave          ",
                    City    => "Commerce            ",
                    Zipcode => "67890     "));

   Put_Line("Employee E:");
   Put_Line("  Name: " & E.Name);
   Put_Line("  Home Address:");
   Put_Line("    " & E.Home.Street);
   Put_Line("    " & E.Home.City & " " & E.Home.Zipcode);
   Put_Line("  Office Address:");
   Put_Line("    " & E.Office.Street);
   Put_Line("    " & E.Office.City & " " & E.Office.Zipcode);
end Nested_Records;
