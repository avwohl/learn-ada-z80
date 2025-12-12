-- Example: basic_access.adb
-- Concept: Access types (pointers)
--
-- Access types are Ada's pointers.
-- They point to dynamically allocated objects.
-- Use "new" to allocate, ".all" to dereference.
-- Ada has automatic initialization to null.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Basic_Access is

   -- Access type declaration
   type Int_Ptr is access Integer;
   type String_Ptr is access String;

   -- Declare access variables
   P : Int_Ptr;      -- Automatically initialized to null
   Q : Int_Ptr;
   S : String_Ptr;
begin
   Put_Line("Basic access type examples:");

   -- Check for null
   if P = null then
      Put_Line("P is null (default)");
   end if;

   -- Allocate with "new"
   P := new Integer;
   Put_Line("After P := new Integer");
   Put_Line("  P = null is " & Boolean'Image(P = null));

   -- Dereference with ".all"
   P.all := 42;
   Put_Line("  P.all =" & Integer'Image(P.all));

   -- Allocate with initial value
   Q := new Integer'(100);  -- Allocate and initialize
   Put_Line("Q := new Integer'(100)");
   Put_Line("  Q.all =" & Integer'Image(Q.all));

   -- Modify through pointer
   Q.all := Q.all + 50;
   Put_Line("  After Q.all + 50:" & Integer'Image(Q.all));

   -- Access to string
   S := new String'("Hello, Ada!");
   Put_Line("S := new String'(""Hello, Ada!"")");
   Put_Line("  S.all = " & S.all);
   Put_Line("  Length:" & Integer'Image(S.all'Length));

   -- Pointer assignment (both point to same object)
   P := Q;
   Put_Line("After P := Q:");
   Put_Line("  P.all =" & Integer'Image(P.all));
   Put_Line("  Q.all =" & Integer'Image(Q.all));

   P.all := 999;
   Put_Line("After P.all := 999:");
   Put_Line("  P.all =" & Integer'Image(P.all));
   Put_Line("  Q.all =" & Integer'Image(Q.all) & " (same object)");

   -- Null dereference would raise Constraint_Error
   -- P := null;
   -- Put_Line(Integer'Image(P.all));  -- Raises Constraint_Error!
end Basic_Access;
