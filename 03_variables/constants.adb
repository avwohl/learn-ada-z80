-- Example: constants.adb
-- Concept: Constants in Ada
--
-- Constants are declared with the "constant" keyword.
-- They must be initialized and cannot be changed.
-- Named constants make code more readable and maintainable.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Constants is
   -- Named constants
   Pi           : constant := 3.14159;  -- Universal constant (no type)
   Max_Size     : constant Integer := 100;
   Min_Size     : constant Integer := 1;
   Greeting     : constant String := "Hello, Ada!";
   Debug_Mode   : constant Boolean := True;

   -- Constants with computed values
   Double_Max   : constant Integer := Max_Size * 2;

   -- The difference: variables can change, constants cannot
   Counter      : Integer := 0;
begin
   Put_Line("Constants:");
   Put_Line("  Max_Size =" & Integer'Image(Max_Size));
   Put_Line("  Min_Size =" & Integer'Image(Min_Size));
   Put_Line("  Double_Max =" & Integer'Image(Double_Max));
   Put_Line("  Greeting = " & Greeting);
   Put_Line("  Debug_Mode = " & Boolean'Image(Debug_Mode));

   -- Can modify variables
   Counter := Counter + 1;
   Put_Line("  Counter (variable) =" & Integer'Image(Counter));

   -- Cannot modify constants:
   -- Max_Size := 200;  -- Error! Constants cannot be changed

   -- Using constants in expressions
   if Counter < Max_Size then
      Put_Line("Counter is within bounds");
   end if;

   Put_Line("Constants improve code readability and safety.");
end Constants;
