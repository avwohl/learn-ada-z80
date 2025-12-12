-- Example: contract_examples.adb
-- Concept: Contract-based programming (Ada 2012)
--
-- Contracts specify preconditions, postconditions,
-- and type invariants. They document and verify
-- the expected behavior of code.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Contract_Examples is

   -- Function with Pre and Post conditions
   function Divide(A, B : Integer) return Integer
     with Pre  => B /= 0,  -- Precondition: B must not be zero
          Post => Divide'Result * B <= A and
                  Divide'Result * B > A - B  -- Postcondition
   is
   begin
      return A / B;
   end Divide;

   -- Procedure with contract
   procedure Increment(X : in Out Integer)
     with Pre  => X < Integer'Last,  -- Room to increment
          Post => X = X'Old + 1      -- X'Old is value before call
   is
   begin
      X := X + 1;
   end Increment;

   -- Function with multiple conditions
   function Factorial(N : Natural) return Positive
     with Pre  => N <= 12,           -- Prevent overflow
          Post => (if N = 0 then Factorial'Result = 1
                   else Factorial'Result >= N)
   is
      Result : Positive := 1;
   begin
      for I in 2 .. N loop
         Result := Result * I;
      end loop;
      return Result;
   end Factorial;

   -- Subtype with predicate (type invariant)
   subtype Positive_Even is Integer
     with Static_Predicate => Positive_Even > 0 and
                              Positive_Even mod 2 = 0;

   -- Dynamic predicate
   subtype Valid_Percentage is Integer
     with Dynamic_Predicate => Valid_Percentage in 0 .. 100;

   V : Integer;
   P : Valid_Percentage;
begin
   Put_Line("Contract examples:");

   -- Test Divide
   Put_Line("Divide function:");
   V := Divide(17, 5);
   Put_Line("  Divide(17, 5) =" & Integer'Image(V));

   -- This would violate precondition:
   -- V := Divide(10, 0);  -- Pre: B /= 0 fails

   -- Test Increment
   Put_Line("Increment procedure:");
   V := 10;
   Increment(V);
   Put_Line("  After Increment(10): V =" & Integer'Image(V));

   -- Test Factorial
   Put_Line("Factorial function:");
   Put_Line("  Factorial(5) =" & Positive'Image(Factorial(5)));
   Put_Line("  Factorial(10) =" & Positive'Image(Factorial(10)));

   -- This would violate precondition:
   -- Put_Line(Positive'Image(Factorial(15)));  -- Pre: N <= 12 fails

   -- Subtype with predicate
   Put_Line("Predicates:");
   P := 50;  -- Valid
   Put_Line("  Valid_Percentage := 50 OK");

   -- This would violate predicate:
   -- P := 150;  -- Predicate fails: not in 0..100

   Put_Line("Contracts help ensure correctness!");
end Contract_Examples;
