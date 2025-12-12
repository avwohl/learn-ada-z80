-- Example: null_type.adb
-- Concept: The null literal and null records
--
-- null is used for:
--   - Access types (pointers) with no target
--   - Null records (empty records)
--   - Null statements (do nothing)

with Ada.Text_IO;
use Ada.Text_IO;

procedure Null_Type is

   -- Access type (pointer)
   type Int_Ptr is access Integer;

   P : Int_Ptr;  -- Automatically initialized to null

   -- Null record (record with no components)
   type Empty_Record is null record;

   E : Empty_Record;

   -- Record that might have no variant fields
   type Optional(Has_Value : Boolean := False) is record
      case Has_Value is
         when True =>
            Value : Integer;
         when False =>
            null;  -- No fields in this variant
      end case;
   end record;

   O1 : Optional(True) := (Has_Value => True, Value => 42);
   O2 : Optional(False) := (Has_Value => False);

begin
   Put_Line("Null examples:");

   -- Null access (pointer)
   Put_Line("Access type:");
   if P = null then
      Put_Line("  P is null (default)");
   end if;

   P := new Integer'(100);
   Put_Line("  After allocation: P.all =" & Integer'Image(P.all));

   P := null;  -- Set back to null
   if P = null then
      Put_Line("  After P := null: P is null again");
   end if;

   -- Null record
   Put_Line("Null record:");
   Put_Line("  Empty_Record has no fields");
   -- E has no fields to access

   -- Optional record with null variant
   Put_Line("Variant record:");
   Put_Line("  O1.Has_Value = " & Boolean'Image(O1.Has_Value));
   Put_Line("  O1.Value =" & Integer'Image(O1.Value));
   Put_Line("  O2.Has_Value = " & Boolean'Image(O2.Has_Value));
   -- O2 has no Value field (it's the null variant)

   -- Null statement
   Put_Line("Null statement:");
   null;  -- Do nothing explicitly
   Put_Line("  (null statement executed - did nothing)");
end Null_Type;
