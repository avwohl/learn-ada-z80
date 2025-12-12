-- Example: parameter_modes.adb
-- Concept: Parameter passing modes
--
-- Ada has three parameter modes:
--   in     - Read-only, value passed in (default)
--   out    - Write-only, value passed back
--   in out - Read-write, value modified in place
--
-- Mode determines what the subprogram can do with the parameter.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Parameter_Modes is

   -- "in" mode: read-only (cannot modify)
   procedure Show_Value(X : in Integer) is
   begin
      Put_Line("  Value is:" & Integer'Image(X));
      -- X := 10;  -- Error! Cannot assign to "in" parameter
   end Show_Value;

   -- "out" mode: write-only (must assign before procedure ends)
   procedure Initialize(X : out Integer) is
   begin
      -- Put_Line(Integer'Image(X));  -- Value may be undefined
      X := 100;  -- Must assign a value
   end Initialize;

   -- "in out" mode: read and write
   procedure Double(X : in out Integer) is
   begin
      Put_Line("  Received:" & Integer'Image(X));
      X := X * 2;  -- Can read and modify
      Put_Line("  Doubled to:" & Integer'Image(X));
   end Double;

   -- Multiple parameters with different modes
   procedure Divide(Dividend  : in Integer;
                    Divisor   : in Integer;
                    Quotient  : out Integer;
                    Remainder : out Integer) is
   begin
      Quotient := Dividend / Divisor;
      Remainder := Dividend mod Divisor;
   end Divide;

   -- Array parameters
   type Int_Array is array(1 .. 5) of Integer;

   procedure Fill_Array(A : out Int_Array; Value : in Integer) is
   begin
      for I in A'Range loop
         A(I) := Value;
      end loop;
   end Fill_Array;

   procedure Sum_Array(A : in Int_Array; Total : out Integer) is
   begin
      Total := 0;
      for I in A'Range loop
         Total := Total + A(I);
      end loop;
   end Sum_Array;

   N, Q, R : Integer;
   Arr : Int_Array;
begin
   Put_Line("Parameter mode examples:");

   -- "in" mode
   Put_Line("in mode:");
   N := 42;
   Show_Value(N);  -- N is unchanged

   -- "out" mode
   Put_Line("out mode:");
   Initialize(N);
   Put_Line("  After Initialize: N =" & Integer'Image(N));

   -- "in out" mode
   Put_Line("in out mode:");
   N := 50;
   Double(N);
   Put_Line("  Final N =" & Integer'Image(N));

   -- Multiple out parameters
   Put_Line("Multiple out parameters:");
   Divide(17, 5, Q, R);
   Put_Line("  17 / 5 = " & Integer'Image(Q) &
            " remainder" & Integer'Image(R));

   -- Array out parameter
   Put_Line("Array parameters:");
   Fill_Array(Arr, 10);
   Put("  Array after fill:");
   for V of Arr loop
      Put(Integer'Image(V));
   end loop;
   New_Line;

   Sum_Array(Arr, N);
   Put_Line("  Sum of array:" & Integer'Image(N));
end Parameter_Modes;
