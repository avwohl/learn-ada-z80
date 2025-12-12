-- Example: integer_types.adb
-- Concept: Integer types in Ada
--
-- Ada has several predefined integer types:
--   Integer  - Standard integer (16-bit on Z80)
--   Natural  - 0 and positive integers (subtype of Integer)
--   Positive - Positive integers only (subtype of Integer)
--
-- On Z80, Integer is typically 16-bit (-32768 to 32767)

with Ada.Text_IO;
use Ada.Text_IO;

procedure Integer_Types is
   A : Integer  := -100;     -- Can be negative
   B : Natural  := 0;        -- 0 or positive
   C : Positive := 1;        -- Must be positive (>= 1)

   -- Integer literals
   Decimal    : Integer := 255;        -- Decimal
   Hex        : Integer := 16#FF#;     -- Hexadecimal (255)
   Octal      : Integer := 8#377#;     -- Octal (255)
   Binary     : Integer := 2#11111111#; -- Binary (255)

   -- Underscores for readability
   Big_Number : Integer := 10_000;     -- Same as 10000
begin
   Put_Line("Integer types:");
   Put_Line("  A (Integer) =" & Integer'Image(A));
   Put_Line("  B (Natural) =" & Natural'Image(B));
   Put_Line("  C (Positive) =" & Positive'Image(C));

   Put_Line("Number bases:");
   Put_Line("  Decimal =" & Integer'Image(Decimal));
   Put_Line("  Hex 16#FF# =" & Integer'Image(Hex));
   Put_Line("  Octal 8#377# =" & Integer'Image(Octal));
   Put_Line("  Binary =" & Integer'Image(Binary));
   Put_Line("  Big_Number =" & Integer'Image(Big_Number));
end Integer_Types;
