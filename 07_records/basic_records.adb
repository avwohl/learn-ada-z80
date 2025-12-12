-- Example: basic_records.adb
-- Concept: Record types (structs)
--
-- Records group related data of different types.
-- Each field has a name and type.
-- Access fields with dot notation: Record.Field

with Ada.Text_IO;
use Ada.Text_IO;

procedure Basic_Records is
   -- Simple record type
   type Point is record
      X : Integer;
      Y : Integer;
   end record;

   -- Record with different field types
   type Person is record
      Name   : String(1 .. 20);
      Age    : Natural;
      Active : Boolean;
   end record;

   -- Record with default values
   type Counter is record
      Value : Integer := 0;
      Max   : Integer := 100;
   end record;

   P1, P2 : Point;
   Employee : Person;
   C : Counter;  -- Gets default values
begin
   Put_Line("Basic record examples:");

   -- Assign to fields individually
   P1.X := 10;
   P1.Y := 20;

   Put_Line("Point P1:");
   Put_Line("  X =" & Integer'Image(P1.X));
   Put_Line("  Y =" & Integer'Image(P1.Y));

   -- Aggregate initialization
   P2 := (X => 30, Y => 40);
   Put_Line("Point P2 (from aggregate):");
   Put_Line("  X =" & Integer'Image(P2.X));
   Put_Line("  Y =" & Integer'Image(P2.Y));

   -- Record with string field
   Employee := (Name   => "Alice               ",  -- Pad to 20 chars
                Age    => 30,
                Active => True);

   Put_Line("Employee record:");
   Put_Line("  Name = " & Employee.Name);
   Put_Line("  Age =" & Natural'Image(Employee.Age));
   Put_Line("  Active = " & Boolean'Image(Employee.Active));

   -- Modify a field
   Employee.Age := 31;
   Put_Line("After birthday: Age =" & Natural'Image(Employee.Age));

   -- Record with defaults
   Put_Line("Counter with defaults:");
   Put_Line("  Value =" & Integer'Image(C.Value));
   Put_Line("  Max =" & Integer'Image(C.Max));

   -- Record assignment (copies all fields)
   P1 := P2;
   Put_Line("After P1 := P2, P1.X =" & Integer'Image(P1.X));

   -- Record comparison
   if P1 = P2 then
      Put_Line("P1 equals P2");
   end if;
end Basic_Records;
