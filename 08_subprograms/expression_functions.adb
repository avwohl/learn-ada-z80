-- Example: expression_functions.adb
-- Concept: Expression functions (Ada 2012)
--
-- Short functions can be written as single expressions.
-- Syntax: function Name(Params) return Type is (Expression);
-- More concise for simple computations.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Expression_Functions is

   -- Traditional function
   function Square_Traditional(N : Integer) return Integer is
   begin
      return N * N;
   end Square_Traditional;

   -- Same as expression function (much shorter!)
   function Square(N : Integer) return Integer is (N * N);

   -- More expression functions
   function Cube(N : Integer) return Integer is (N * N * N);

   function Max(A, B : Integer) return Integer is
     (if A > B then A else B);

   function Min(A, B : Integer) return Integer is
     (if A < B then A else B);

   function Abs_Value(N : Integer) return Integer is
     (if N < 0 then -N else N);

   function Is_Positive(N : Integer) return Boolean is (N > 0);

   function Is_Even(N : Integer) return Boolean is (N mod 2 = 0);

   function Clamp(Value, Low, High : Integer) return Integer is
     (Max(Low, Min(Value, High)));

   -- With subtypes
   subtype Percentage is Integer range 0 .. 100;

   function To_Percentage(N : Integer) return Percentage is
     (Percentage(Clamp(N, 0, 100)));

   -- Using case expression
   type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

   function Is_Weekend(D : Day) return Boolean is
     (case D is
        when Sat | Sun => True,
        when others => False);

   function Day_Type(D : Day) return String is
     (case D is
        when Mon .. Fri => "Weekday",
        when Sat | Sun => "Weekend");
begin
   Put_Line("Expression function examples:");

   Put_Line("Math functions:");
   Put_Line("  Square(5) =" & Integer'Image(Square(5)));
   Put_Line("  Cube(3) =" & Integer'Image(Cube(3)));
   Put_Line("  Max(10, 20) =" & Integer'Image(Max(10, 20)));
   Put_Line("  Min(10, 20) =" & Integer'Image(Min(10, 20)));
   Put_Line("  Abs_Value(-42) =" & Integer'Image(Abs_Value(-42)));

   Put_Line("Boolean functions:");
   Put_Line("  Is_Positive(5) = " & Boolean'Image(Is_Positive(5)));
   Put_Line("  Is_Positive(-5) = " & Boolean'Image(Is_Positive(-5)));
   Put_Line("  Is_Even(4) = " & Boolean'Image(Is_Even(4)));
   Put_Line("  Is_Even(5) = " & Boolean'Image(Is_Even(5)));

   Put_Line("Clamp function:");
   Put_Line("  Clamp(150, 0, 100) =" & Integer'Image(Clamp(150, 0, 100)));
   Put_Line("  Clamp(-10, 0, 100) =" & Integer'Image(Clamp(-10, 0, 100)));
   Put_Line("  Clamp(50, 0, 100) =" & Integer'Image(Clamp(50, 0, 100)));

   Put_Line("Day functions:");
   Put_Line("  Is_Weekend(Sat) = " & Boolean'Image(Is_Weekend(Sat)));
   Put_Line("  Is_Weekend(Mon) = " & Boolean'Image(Is_Weekend(Mon)));
   Put_Line("  Day_Type(Wed) = " & Day_Type(Wed));
   Put_Line("  Day_Type(Sun) = " & Day_Type(Sun));
end Expression_Functions;
