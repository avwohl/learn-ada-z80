-- Example: modular_types.adb
-- Concept: Modular (unsigned wraparound) types
--
-- Modular types are unsigned integers that wrap around
-- instead of raising an exception on overflow.
-- They're useful for:
--   - Bit manipulation
--   - Circular buffers
--   - Hash calculations

with Ada.Text_IO;
use Ada.Text_IO;

procedure Modular_Types is
   -- Modular types: unsigned with wraparound
   type Byte is mod 256;        -- 0..255, wraps around
   type Word is mod 65536;      -- 0..65535
   type Nibble is mod 16;       -- 0..15

   B : Byte := 250;
   N : Nibble := 14;
begin
   Put_Line("Modular types demonstrate wraparound:");

   Put_Line("Byte starts at: " & Byte'Image(B));
   B := B + 10;  -- Would be 260, wraps to 4
   Put_Line("After adding 10: " & Byte'Image(B));

   Put_Line("Nibble starts at: " & Nibble'Image(N));
   N := N + 5;   -- Would be 19, wraps to 3
   Put_Line("After adding 5: " & Nibble'Image(N));

   -- Bit operations work naturally on modular types
   declare
      X : Byte := 16#AA#;  -- 10101010 in binary
      Y : Byte := 16#55#;  -- 01010101 in binary
   begin
      Put_Line("Bit operations:");
      Put_Line("  X = " & Byte'Image(X));
      Put_Line("  Y = " & Byte'Image(Y));
      Put_Line("  X and Y = " & Byte'Image(X and Y));  -- 0
      Put_Line("  X or Y = " & Byte'Image(X or Y));    -- 255
      Put_Line("  X xor Y = " & Byte'Image(X xor Y));  -- 255
      Put_Line("  not X = " & Byte'Image(not X));      -- 85
   end;
end Modular_Types;
