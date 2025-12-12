-- Example: fixed_strings.adb
-- Concept: Fixed-length strings
--
-- String is an array of Character with fixed bounds.
-- The length is determined at declaration and cannot change.
-- Use Ada.Strings.Fixed for string manipulation.

with Ada.Text_IO;
with Ada.Strings.Fixed;
use Ada.Text_IO;

procedure Fixed_Strings is
   -- Fixed-length strings
   Name   : String(1 .. 20) := "John                ";  -- Must be 20 chars
   Buffer : String(1 .. 50) := (others => ' ');        -- 50 spaces

   -- String literals
   Greeting : constant String := "Hello";  -- Length determined by literal
begin
   Put_Line("Fixed string examples:");

   -- String attributes
   Put_Line("Name attributes:");
   Put_Line("  Name'First =" & Integer'Image(Name'First));
   Put_Line("  Name'Last =" & Integer'Image(Name'Last));
   Put_Line("  Name'Length =" & Integer'Image(Name'Length));

   -- String content
   Put_Line("  Name = """ & Name & """");

   -- Using Ada.Strings.Fixed
   Put_Line("Ada.Strings.Fixed operations:");

   -- Trim removes leading/trailing spaces
   Put_Line("  Trimmed: """ &
            Ada.Strings.Fixed.Trim(Name, Ada.Strings.Both) & """");

   -- Head/Tail
   Put_Line("  Head(Name, 4) = """ &
            Ada.Strings.Fixed.Head(Name, 4) & """");
   Put_Line("  Tail(Name, 4) = """ &
            Ada.Strings.Fixed.Tail(Name, 4) & """");

   -- Index (find substring)
   declare
      Pos : Natural;
   begin
      Pos := Ada.Strings.Fixed.Index(Name, "oh");
      Put_Line("  Index(Name, ""oh"") =" & Natural'Image(Pos));
   end;

   -- Move (copy with padding)
   Ada.Strings.Fixed.Move("Alice", Name);
   Put_Line("  After Move(""Alice"", Name): """ & Name & """");

   -- Overwrite
   Ada.Strings.Fixed.Overwrite(Buffer, 1, "Hello, World!");
   Put_Line("  Buffer after Overwrite: """ &
            Ada.Strings.Fixed.Trim(Buffer, Ada.Strings.Right) & """");

   -- String slicing
   Put_Line("String slicing:");
   declare
      S : String := "ABCDEFGHIJ";
   begin
      Put_Line("  S = """ & S & """");
      Put_Line("  S(1..3) = """ & S(1 .. 3) & """");
      Put_Line("  S(4..7) = """ & S(4 .. 7) & """");
   end;

   -- Concatenation
   Put_Line("Concatenation:");
   declare
      A : constant String := "Hello";
      B : constant String := "World";
      C : constant String := A & " " & B;
   begin
      Put_Line("  """ & A & """ & "" "" & """ & B & """ = """ & C & """");
   end;
end Fixed_Strings;
