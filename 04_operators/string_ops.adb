-- Example: string_ops.adb
-- Concept: String concatenation operator
--
-- The "&" operator concatenates strings and characters.
-- Ada strings are fixed-length arrays of characters.

with Ada.Text_IO;
use Ada.Text_IO;

procedure String_Ops is
   First  : constant String := "Hello";
   Last   : constant String := "World";
   Space  : constant Character := ' ';

   Result : String(1..11);
begin
   Put_Line("String concatenation with &:");

   -- Concatenate strings
   Put_Line("  First & Last = " & First & Last);

   -- Concatenate with character
   Put_Line("  First & Space & Last = " & First & Space & Last);

   -- Building strings
   Result := First & Space & Last;
   Put_Line("  Result = " & Result);

   -- Character concatenation
   Put_Line("  'A' & 'B' & 'C' = " & 'A' & 'B' & 'C');

   -- Concatenating with 'Image for numbers
   declare
      N : Integer := 42;
   begin
      Put_Line("  Number " & Integer'Image(N) & " is the answer");
   end;

   -- String slicing
   declare
      S : constant String := "Hello, Ada!";
   begin
      Put_Line("String slicing:");
      Put_Line("  Full string: " & S);
      Put_Line("  S(1..5) = " & S(1..5));
      Put_Line("  S(8..10) = " & S(8..10));
   end;

   -- String comparison (lexicographic)
   declare
      A : constant String := "Apple";
      B : constant String := "Banana";
   begin
      Put_Line("String comparison:");
      Put_Line("  ""Apple"" < ""Banana"" is " &
               Boolean'Image(A < B));
      Put_Line("  ""Apple"" = ""Apple"" is " &
               Boolean'Image(A = "Apple"));
   end;
end String_Ops;
