-- Example: if_expression.adb
-- Concept: If expressions (Ada 2012)
--
-- If expressions return values (like ternary ?: in C).
-- Syntax: (if Condition then Value1 else Value2)
-- All branches must return same type.

with Ada.Text_IO;
use Ada.Text_IO;

procedure If_Expression is
   A : Integer := 10;
   B : Integer := 20;
   Max : Integer;
   Min : Integer;

   function Sign(N : Integer) return Integer is
     (if N > 0 then 1 elsif N < 0 then -1 else 0);

   function Abs_Value(N : Integer) return Integer is
     (if N < 0 then -N else N);
begin
   Put_Line("If expression examples (Ada 2012):");

   -- Basic if expression
   Max := (if A > B then A else B);
   Put_Line("Max of" & Integer'Image(A) & " and" &
            Integer'Image(B) & " is" & Integer'Image(Max));

   Min := (if A < B then A else B);
   Put_Line("Min is" & Integer'Image(Min));

   -- In function result
   Put_Line("Sign(-5) =" & Integer'Image(Sign(-5)));
   Put_Line("Sign(0) =" & Integer'Image(Sign(0)));
   Put_Line("Sign(10) =" & Integer'Image(Sign(10)));

   Put_Line("Abs(-42) =" & Integer'Image(Abs_Value(-42)));

   -- If expression in Put_Line
   Put_Line("A is " & (if A mod 2 = 0 then "even" else "odd"));
   Put_Line("B is " & (if B mod 2 = 0 then "even" else "odd"));

   -- Nested if expressions
   declare
      Score : Integer := 85;
      Grade : Character;
   begin
      Grade := (if Score >= 90 then 'A'
                elsif Score >= 80 then 'B'
                elsif Score >= 70 then 'C'
                elsif Score >= 60 then 'D'
                else 'F');
      Put_Line("Score" & Integer'Image(Score) & " = Grade " & Grade);
   end;

   -- If expression with Boolean result
   declare
      X : Integer := 5;
      In_Range : Boolean;
   begin
      In_Range := (if X >= 1 and X <= 10 then True else False);
      Put_Line("X in 1..10: " & Boolean'Image(In_Range));

      -- Simpler: just use the condition directly
      In_Range := X >= 1 and X <= 10;
   end;
end If_Expression;
