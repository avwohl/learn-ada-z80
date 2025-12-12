-- Example: procedures.adb
-- Concept: Procedures - subprograms that don't return values
--
-- Procedures perform actions but don't return a value.
-- They are called as statements.
-- Parameters can be in, out, or in out mode.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Procedures is

   -- Simple procedure with no parameters
   procedure Say_Hello is
   begin
      Put_Line("Hello from procedure!");
   end Say_Hello;

   -- Procedure with input parameter (default mode is "in")
   procedure Print_Number(N : Integer) is
   begin
      Put_Line("Number:" & Integer'Image(N));
   end Print_Number;

   -- Procedure with explicit "in" parameter
   procedure Print_Twice(Value : in Integer) is
   begin
      Put_Line("Value:" & Integer'Image(Value) &
               " Doubled:" & Integer'Image(Value * 2));
   end Print_Twice;

   -- Procedure with "out" parameter (returns a value through parameter)
   procedure Get_Next_Value(Current : Integer; Next : out Integer) is
   begin
      Next := Current + 1;
   end Get_Next_Value;

   -- Procedure with "in out" parameter (modifies parameter)
   procedure Increment(Value : in out Integer) is
   begin
      Value := Value + 1;
   end Increment;

   -- Procedure with multiple parameters
   procedure Swap(A, B : in out Integer) is
      Temp : Integer;
   begin
      Temp := A;
      A := B;
      B := Temp;
   end Swap;

   X, Y, Z : Integer;
begin
   Put_Line("Procedure examples:");

   -- Call simple procedure
   Say_Hello;

   -- Call with parameter
   Print_Number(42);
   Print_Twice(25);

   -- Call with out parameter
   X := 10;
   Get_Next_Value(X, Y);
   Put_Line("After Get_Next_Value(10, Y): Y =" & Integer'Image(Y));

   -- Call with in out parameter
   Z := 100;
   Put_Line("Before Increment: Z =" & Integer'Image(Z));
   Increment(Z);
   Put_Line("After Increment: Z =" & Integer'Image(Z));

   -- Swap example
   X := 5;
   Y := 10;
   Put_Line("Before Swap: X=" & Integer'Image(X) & " Y=" & Integer'Image(Y));
   Swap(X, Y);
   Put_Line("After Swap: X=" & Integer'Image(X) & " Y=" & Integer'Image(Y));
end Procedures;
