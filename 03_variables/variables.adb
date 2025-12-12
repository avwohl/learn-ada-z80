-- Example: variables.adb
-- Concept: Variable declarations in Ada
--
-- Variables are declared in the declarative part
-- (between "is" and "begin").
-- Syntax: Name : Type := Initial_Value;
-- The initial value is optional.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Variables is
   -- Variables with initial values
   Count    : Integer := 0;
   Total    : Integer := 100;
   Name     : String(1..10) := "Ada       ";
   Active   : Boolean := True;
   Grade    : Character := 'A';

   -- Variables without initial values (will be undefined until assigned)
   Result   : Integer;
   Flag     : Boolean;

   -- Multiple variables of same type
   X, Y, Z  : Integer := 0;  -- All initialized to 0
begin
   Put_Line("Initial values:");
   Put_Line("  Count =" & Integer'Image(Count));
   Put_Line("  Total =" & Integer'Image(Total));
   Put_Line("  Active = " & Boolean'Image(Active));
   Put("  Grade = ");
   Put(Grade);
   New_Line;

   -- Assignment statements
   Count := Count + 1;
   Result := Total * 2;
   Flag := Count > 0;

   Put_Line("After assignments:");
   Put_Line("  Count =" & Integer'Image(Count));
   Put_Line("  Result =" & Integer'Image(Result));
   Put_Line("  Flag = " & Boolean'Image(Flag));

   -- Multiple assignment targets share same type
   X := 10;
   Y := 20;
   Z := X + Y;
   Put_Line("  X=" & Integer'Image(X) &
            " Y=" & Integer'Image(Y) &
            " Z=" & Integer'Image(Z));
end Variables;
