-- Example: package_intro.adb
-- Concept: Introduction to packages
--
-- Packages are Ada's main modular programming unit.
-- They encapsulate types, variables, and subprograms.
-- Packages have a specification (interface) and body (implementation).
-- This example shows embedded packages for demonstration.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Package_Intro is

   -- An embedded package specification
   package Counter is
      procedure Reset;
      procedure Increment;
      procedure Decrement;
      function Value return Integer;
   end Counter;

   -- The package body (implementation)
   package body Counter is
      Current_Value : Integer := 0;  -- Private to package

      procedure Reset is
      begin
         Current_Value := 0;
      end Reset;

      procedure Increment is
      begin
         Current_Value := Current_Value + 1;
      end Increment;

      procedure Decrement is
      begin
         Current_Value := Current_Value - 1;
      end Decrement;

      function Value return Integer is
      begin
         return Current_Value;
      end Value;
   end Counter;

   -- Another embedded package
   package Math is
      function Square(N : Integer) return Integer;
      function Cube(N : Integer) return Integer;
      Pi : constant := 3.14159;
   end Math;

   package body Math is
      function Square(N : Integer) return Integer is
      begin
         return N * N;
      end Square;

      function Cube(N : Integer) return Integer is
      begin
         return N * N * N;
      end Cube;
   end Math;

begin
   Put_Line("Package introduction:");

   -- Use Counter package
   Put_Line("Counter package:");
   Put_Line("  Initial value:" & Integer'Image(Counter.Value));

   Counter.Increment;
   Counter.Increment;
   Counter.Increment;
   Put_Line("  After 3 increments:" & Integer'Image(Counter.Value));

   Counter.Decrement;
   Put_Line("  After decrement:" & Integer'Image(Counter.Value));

   Counter.Reset;
   Put_Line("  After reset:" & Integer'Image(Counter.Value));

   -- Use Math package
   Put_Line("Math package:");
   Put_Line("  Square(5) =" & Integer'Image(Math.Square(5)));
   Put_Line("  Cube(3) =" & Integer'Image(Math.Cube(3)));

   -- Note: Current_Value is NOT accessible from here
   -- It's private to the Counter package body
   -- Counter.Current_Value := 100;  -- Error! Not visible
end Package_Intro;
