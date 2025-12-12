-- Example: private_types.adb
-- Concept: Private types in packages
--
-- Private types hide implementation details.
-- Clients can use the type but not access its internals.
-- The "private" section reveals implementation to compiler.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Private_Types is

   -- Package with private type
   package Rational is
      type Fraction is private;

      -- Operations on Fraction (the public interface)
      function Create(Num, Denom : Integer) return Fraction;
      function Numerator(F : Fraction) return Integer;
      function Denominator(F : Fraction) return Integer;
      function "+"(Left, Right : Fraction) return Fraction;
      function "*"(Left, Right : Fraction) return Fraction;
      function Image(F : Fraction) return String;

   private
      -- Actual implementation (hidden from clients)
      type Fraction is record
         Num   : Integer := 0;
         Denom : Integer := 1;
      end record;
   end Rational;

   package body Rational is
      function GCD(A, B : Integer) return Integer is
         X : Integer := abs(A);
         Y : Integer := abs(B);
      begin
         while Y /= 0 loop
            declare
               T : constant Integer := Y;
            begin
               Y := X mod Y;
               X := T;
            end;
         end loop;
         return X;
      end GCD;

      function Create(Num, Denom : Integer) return Fraction is
         G : Integer;
         N : Integer := Num;
         D : Integer := Denom;
      begin
         if D < 0 then
            N := -N;
            D := -D;
         end if;
         G := GCD(N, D);
         return (Num => N / G, Denom => D / G);
      end Create;

      function Numerator(F : Fraction) return Integer is
      begin
         return F.Num;
      end Numerator;

      function Denominator(F : Fraction) return Integer is
      begin
         return F.Denom;
      end Denominator;

      function "+"(Left, Right : Fraction) return Fraction is
      begin
         return Create(Left.Num * Right.Denom + Right.Num * Left.Denom,
                       Left.Denom * Right.Denom);
      end "+";

      function "*"(Left, Right : Fraction) return Fraction is
      begin
         return Create(Left.Num * Right.Num, Left.Denom * Right.Denom);
      end "*";

      function Image(F : Fraction) return String is
      begin
         return Integer'Image(F.Num) & "/" & Integer'Image(F.Denom);
      end Image;
   end Rational;

   use Rational;

   A, B, C : Fraction;
begin
   Put_Line("Private type examples:");

   -- Create fractions
   A := Create(1, 2);  -- 1/2
   B := Create(2, 3);  -- 2/3

   Put_Line("A =" & Image(A));
   Put_Line("B =" & Image(B));

   -- Use operations
   C := A + B;
   Put_Line("A + B =" & Image(C));

   C := A * B;
   Put_Line("A * B =" & Image(C));

   -- Access via functions only
   Put_Line("Numerator(A) =" & Integer'Image(Numerator(A)));
   Put_Line("Denominator(A) =" & Integer'Image(Denominator(A)));

   -- Cannot access record fields directly:
   -- A.Num := 5;  -- Error! Fraction is private

   -- Auto-reduces fractions
   C := Create(4, 6);
   Put_Line("Create(4, 6) =" & Image(C) & " (reduced)");
end Private_Types;
