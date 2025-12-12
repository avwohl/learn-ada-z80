-- Example: overloading.adb
-- Concept: Subprogram overloading
--
-- Ada allows multiple subprograms with the same name
-- if they differ in parameter types or number.
-- The compiler chooses based on the call context.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Overloading is

   -- Print for Integer
   procedure Print(Value : Integer) is
   begin
      Put_Line("Integer:" & Integer'Image(Value));
   end Print;

   -- Print for Boolean
   procedure Print(Value : Boolean) is
   begin
      Put_Line("Boolean: " & Boolean'Image(Value));
   end Print;

   -- Print for Character
   procedure Print(Value : Character) is
   begin
      Put("Character: ");
      Put(Value);
      New_Line;
   end Print;

   -- Print for String
   procedure Print(Value : String) is
   begin
      Put_Line("String: " & Value);
   end Print;

   -- Same name, different number of parameters
   function Add(A, B : Integer) return Integer is
   begin
      return A + B;
   end Add;

   function Add(A, B, C : Integer) return Integer is
   begin
      return A + B + C;
   end Add;

   -- Overloaded by return type (rare, needs context)
   type Byte is mod 256;

   function Convert(S : String) return Integer is
      Result : Integer := 0;
   begin
      for C of S loop
         if C in '0' .. '9' then
            Result := Result * 10 + (Character'Pos(C) - Character'Pos('0'));
         end if;
      end loop;
      return Result;
   end Convert;

   function Convert(S : String) return Byte is
   begin
      return Byte(Convert(S) mod 256);  -- Calls Integer version
   end Convert;

   I : Integer;
   B : Byte;
begin
   Put_Line("Overloading examples:");

   -- Compiler chooses based on argument type
   Put_Line("Print overloads:");
   Print(42);         -- Calls Print(Integer)
   Print(True);       -- Calls Print(Boolean)
   Print('X');        -- Calls Print(Character)
   Print("Hello");    -- Calls Print(String)

   -- Different parameter counts
   Put_Line("Add overloads:");
   Put_Line("  Add(2, 3) =" & Integer'Image(Add(2, 3)));
   Put_Line("  Add(2, 3, 4) =" & Integer'Image(Add(2, 3, 4)));

   -- Return type overloading (context determines which)
   Put_Line("Return type overloading:");
   I := Convert("1234");  -- Calls Integer version
   B := Convert("1234");  -- Calls Byte version
   Put_Line("  As Integer:" & Integer'Image(I));
   Put_Line("  As Byte:" & Byte'Image(B));

   -- Operators can also be overloaded
   Put_Line("Operators are overloaded by default:");
   Put_Line("  3 + 5 =" & Integer'Image(3 + 5));  -- Integer +
   Put_Line("  ""Hel"" & ""lo"" = " & "Hel" & "lo");  -- String &
end Overloading;
