-- Example: integer_io.adb
-- Concept: Integer I/O
--
-- Ada.Text_IO.Integer_IO provides formatted integer I/O.
-- Instantiate for your integer type.
-- Control width, base, and formatting.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Integer_IO is
   -- Instantiate Integer_IO for Integer type
   package Int_IO is new Ada.Text_IO.Integer_IO(Integer);
   use Int_IO;

   -- For other integer types
   type Byte is range 0 .. 255;
   package Byte_IO is new Ada.Text_IO.Integer_IO(Byte);

   N : Integer := 12345;
   B : Byte := 200;
begin
   Put_Line("Integer I/O examples:");

   -- Basic output
   Put("Default format: ");
   Put(N);
   New_Line;

   -- Specified width (right-justified)
   Put("Width 10: '");
   Put(N, Width => 10);
   Put_Line("'");

   -- Minimum width
   Put("Width 1: '");
   Put(N, Width => 1);
   Put_Line("'");

   -- Different bases
   Put_Line("Different bases for 255:");

   Put("  Decimal (base 10): ");
   Put(255, Base => 10);
   New_Line;

   Put("  Hex (base 16): ");
   Put(255, Base => 16);
   New_Line;

   Put("  Octal (base 8): ");
   Put(255, Base => 8);
   New_Line;

   Put("  Binary (base 2): ");
   Put(255, Base => 2);
   New_Line;

   -- Using Byte_IO
   Put("Byte value: ");
   Byte_IO.Put(B, Width => 5);
   New_Line;

   -- Negative numbers
   Put("Negative: ");
   Put(-42, Width => 6);
   New_Line;

   -- Using 'Image as alternative
   Put_Line("Using 'Image:");
   Put_Line("  Integer'Image(N) =" & Integer'Image(N));

   -- Note: Get reads integers from input
   -- Int_IO.Get(N);  -- Would read integer from console
end Integer_IO;
