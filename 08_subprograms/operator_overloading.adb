-- Example: operator_overloading.adb
-- Concept: Overloading operators for custom types
--
-- Ada allows defining operators (+, -, *, etc.) for your types.
-- Makes custom types behave like built-in types.
-- Operators are just functions with special names.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Operator_Overloading is

   -- Complex number type
   type Complex is record
      Re : Integer;  -- Real part
      Im : Integer;  -- Imaginary part
   end record;

   -- Overload "+" for Complex
   function "+"(Left, Right : Complex) return Complex is
   begin
      return (Re => Left.Re + Right.Re,
              Im => Left.Im + Right.Im);
   end "+";

   -- Overload "-" for Complex
   function "-"(Left, Right : Complex) return Complex is
   begin
      return (Re => Left.Re - Right.Re,
              Im => Left.Im - Right.Im);
   end "-";

   -- Overload "*" for Complex
   function "*"(Left, Right : Complex) return Complex is
   begin
      -- (a+bi)(c+di) = (ac-bd) + (ad+bc)i
      return (Re => Left.Re * Right.Re - Left.Im * Right.Im,
              Im => Left.Re * Right.Im + Left.Im * Right.Re);
   end "*";

   -- Overload unary "-" (negation)
   function "-"(Right : Complex) return Complex is
   begin
      return (Re => -Right.Re, Im => -Right.Im);
   end "-";

   -- Overload "=" (equality) - often automatic but can customize
   function "="(Left, Right : Complex) return Boolean is
   begin
      return Left.Re = Right.Re and Left.Im = Right.Im;
   end "=";

   function Image(C : Complex) return String is
   begin
      if C.Im >= 0 then
         return Integer'Image(C.Re) & " +" & Integer'Image(C.Im) & "i";
      else
         return Integer'Image(C.Re) & " -" & Integer'Image(abs C.Im) & "i";
      end if;
   end Image;

   A, B, C : Complex;
begin
   Put_Line("Operator overloading examples:");
   New_Line;

   A := (Re => 3, Im => 4);    -- 3 + 4i
   B := (Re => 1, Im => 2);    -- 1 + 2i

   Put_Line("A = " & Image(A));
   Put_Line("B = " & Image(B));
   New_Line;

   -- Use overloaded operators
   C := A + B;
   Put_Line("A + B = " & Image(C));

   C := A - B;
   Put_Line("A - B = " & Image(C));

   C := A * B;
   Put_Line("A * B = " & Image(C));

   C := -A;
   Put_Line("-A = " & Image(C));
   New_Line;

   -- Equality test
   Put_Line("A = B: " & Boolean'Image(A = B));
   Put_Line("A = A: " & Boolean'Image(A = A));
   New_Line;

   -- Complex expressions work naturally
   Put_Line("Complex expression: A + B * A");
   C := A + B * A;  -- Uses precedence: A + (B * A)
   Put_Line("Result = " & Image(C));
end Operator_Overloading;
