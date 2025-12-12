-- Example: local_declarations.adb
-- Concept: Local declarations in subprograms
--
-- Subprograms can declare local types, variables,
-- constants, and even nested subprograms.
-- These are only visible within the subprogram.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Local_Declarations is

   procedure Process_Data is
      -- Local constant
      Max_Items : constant := 10;

      -- Local type
      type Item_Array is array(1 .. Max_Items) of Integer;

      -- Local variables
      Items : Item_Array;
      Count : Natural := 0;
      Sum   : Integer := 0;

      -- Local nested procedure
      procedure Add_Item(Value : Integer) is
      begin
         if Count < Max_Items then
            Count := Count + 1;
            Items(Count) := Value;
         end if;
      end Add_Item;

      -- Local nested function
      function Get_Average return Integer is
      begin
         if Count = 0 then
            return 0;
         else
            return Sum / Count;
         end if;
      end Get_Average;

   begin  -- Process_Data
      -- Use local declarations
      for I in 1 .. 5 loop
         Add_Item(I * 10);
         Sum := Sum + I * 10;
      end loop;

      Put_Line("  Count:" & Natural'Image(Count));
      Put_Line("  Sum:" & Integer'Image(Sum));
      Put_Line("  Average:" & Integer'Image(Get_Average));
   end Process_Data;

   function Calculate(N : Integer) return Integer is
      -- Local variables with initialization
      Result   : Integer := 0;
      Multiplier : constant Integer := 2;

      -- Local function
      function Double(X : Integer) return Integer is
      begin
         return X * Multiplier;  -- Can access outer local constant
      end Double;
   begin
      for I in 1 .. N loop
         Result := Result + Double(I);
      end loop;
      return Result;
   end Calculate;
begin
   Put_Line("Local declarations examples:");

   Put_Line("Process_Data:");
   Process_Data;

   Put_Line("Calculate(5):" & Integer'Image(Calculate(5)));

   -- Note: Items, Count, Add_Item, etc. are not visible here
   -- They are local to Process_Data

   -- Shadowing example
   declare
      X : Integer := 100;

      procedure Inner is
         X : Integer := 50;  -- Shadows outer X
      begin
         Put_Line("  Inner X:" & Integer'Image(X));  -- 50
      end Inner;
   begin
      Put_Line("Shadowing:");
      Put_Line("  Outer X:" & Integer'Image(X));  -- 100
      Inner;
      Put_Line("  Outer X still:" & Integer'Image(X));  -- Still 100
   end;
end Local_Declarations;
