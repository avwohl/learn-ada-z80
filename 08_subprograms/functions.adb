-- Example: functions.adb
-- Concept: Functions - subprograms that return values
--
-- Functions compute and return a value.
-- They are called in expressions.
-- Parameters are typically "in" mode only.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Functions is

   -- Simple function with no parameters
   function Get_Magic_Number return Integer is
   begin
      return 42;
   end Get_Magic_Number;

   -- Function with one parameter
   function Square(N : Integer) return Integer is
   begin
      return N * N;
   end Square;

   -- Function with multiple parameters
   function Max(A, B : Integer) return Integer is
   begin
      if A > B then
         return A;
      else
         return B;
      end if;
   end Max;

   -- Function with Boolean return
   function Is_Even(N : Integer) return Boolean is
   begin
      return N mod 2 = 0;
   end Is_Even;

   -- Function with expression (no local variables)
   function Cube(N : Integer) return Integer is (N * N * N);

   -- Function with local variables
   function Factorial(N : Natural) return Natural is
      Result : Natural := 1;
   begin
      for I in 2 .. N loop
         Result := Result * I;
      end loop;
      return Result;
   end Factorial;

   -- Recursive function
   function Fibonacci(N : Natural) return Natural is
   begin
      if N <= 1 then
         return N;
      else
         return Fibonacci(N - 1) + Fibonacci(N - 2);
      end if;
   end Fibonacci;

   X, Y : Integer;
begin
   Put_Line("Function examples:");

   -- Call functions in expressions
   X := Get_Magic_Number;
   Put_Line("Magic number:" & Integer'Image(X));

   X := Square(7);
   Put_Line("Square(7) =" & Integer'Image(X));

   X := Max(15, 23);
   Put_Line("Max(15, 23) =" & Integer'Image(X));

   -- Boolean function in condition
   if Is_Even(10) then
      Put_Line("10 is even");
   end if;

   -- Function call in output
   Put_Line("Cube(3) =" & Integer'Image(Cube(3)));

   -- Factorial
   Put_Line("Factorial(5) =" & Natural'Image(Factorial(5)));

   -- Fibonacci
   Put("Fibonacci sequence: ");
   for I in 0 .. 10 loop
      Put(Natural'Image(Fibonacci(I)));
   end loop;
   New_Line;

   -- Nested function calls
   Y := Max(Square(3), Square(2));  -- Max(9, 4) = 9
   Put_Line("Max(Square(3), Square(2)) =" & Integer'Image(Y));
end Functions;
