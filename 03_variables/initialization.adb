-- Example: initialization.adb
-- Concept: Variable initialization in Ada
--
-- Ada variables can be:
--   - Initialized at declaration
--   - Initialized with expressions
--   - Left uninitialized (value undefined until assigned)
-- Always initialize variables when possible!

with Ada.Text_IO;
use Ada.Text_IO;

procedure Initialization is
   -- Simple initialization
   Count    : Integer := 0;
   Max      : Integer := 100;

   -- Expression initialization
   Sum      : Integer := 10 + 20 + 30;
   Product  : Integer := Max * 2;

   -- Default values for Boolean
   Done     : Boolean := False;
   Ready    : Boolean := True;

   -- Character initialization
   Initial  : Character := 'A';
   Space    : Character := ' ';

   -- Uninitialized (dangerous - value is undefined!)
   Unknown  : Integer;

begin
   Put_Line("Initialized variables:");
   Put_Line("  Count =" & Integer'Image(Count));
   Put_Line("  Max =" & Integer'Image(Max));
   Put_Line("  Sum =" & Integer'Image(Sum));
   Put_Line("  Product =" & Integer'Image(Product));
   Put_Line("  Done = " & Boolean'Image(Done));
   Put_Line("  Ready = " & Boolean'Image(Ready));

   -- Must assign before use
   Unknown := 42;
   Put_Line("  Unknown (after assignment) =" & Integer'Image(Unknown));

   -- Aggregates for array initialization
   declare
      type Int_Array is array(1..5) of Integer;
      A : Int_Array := (1, 2, 3, 4, 5);
      B : Int_Array := (others => 0);  -- All zeros
   begin
      Put_Line("Array initialization:");
      Put("  A =");
      for I in A'Range loop
         Put(Integer'Image(A(I)));
      end loop;
      New_Line;
      Put("  B =");
      for I in B'Range loop
         Put(Integer'Image(B(I)));
      end loop;
      New_Line;
   end;
end Initialization;
