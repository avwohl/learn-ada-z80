-- Example: object_attributes.adb
-- Concept: Object and type representation attributes
--
-- These attributes give information about how objects
-- are stored and represented in memory.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Object_Attributes is
   type Byte is mod 256;
   type Color is (Red, Green, Blue);

   X : Integer := 42;
   B : Byte := 255;
   C : Color := Green;
   S : String(1 .. 10) := "Hello     ";

   type My_Record is record
      A : Integer;
      B : Boolean;
      C : Character;
   end record;

   R : My_Record := (A => 100, B => True, C => 'X');

   -- Aliased for 'Access
   Y : aliased Integer := 100;
begin
   Put_Line("Object and representation attributes:");

   -- Size of objects and types
   Put_Line("'Size (bits):");
   Put_Line("  Integer'Size =" & Integer'Image(Integer'Size));
   Put_Line("  X'Size =" & Integer'Image(X'Size));
   Put_Line("  Boolean'Size =" & Integer'Image(Boolean'Size));
   Put_Line("  Character'Size =" & Integer'Image(Character'Size));
   Put_Line("  Byte'Size =" & Integer'Image(Byte'Size));
   Put_Line("  Color'Size =" & Integer'Image(Color'Size));
   Put_Line("  My_Record'Size =" & Integer'Image(My_Record'Size));
   Put_Line("  R'Size =" & Integer'Image(R'Size));
   Put_Line("  S'Size =" & Integer'Image(S'Size));

   -- Valid attribute (checks if value is valid for type)
   Put_Line("'Valid attribute:");
   Put_Line("  X'Valid = " & Boolean'Image(X'Valid));
   Put_Line("  C'Valid = " & Boolean'Image(C'Valid));

   -- Constrained attribute
   Put_Line("'Constrained (for discriminated records):");
   -- For regular types, always True

   -- Storage_Size
   Put_Line("Storage attributes:");
   Put_Line("  Integer'Storage_Size =" & Integer'Image(Integer'Size / 8) & " bytes");

   -- 'Image of object (Ada 2012)
   Put_Line("'Image of object:");
   Put_Line("  X'Image = " & X'Image);
   Put_Line("  B'Image = " & B'Image);
   Put_Line("  C'Image = " & C'Image);

   -- 'Old and 'Result are for contracts (in postconditions)
   -- 'Access for getting pointer to aliased object
   Put_Line("'Access attribute:");
   declare
      type Int_Ptr is access all Integer;
      P : Int_Ptr := Y'Access;
   begin
      Put_Line("  Y'Access points to value" & Integer'Image(P.all));
   end;
end Object_Attributes;
