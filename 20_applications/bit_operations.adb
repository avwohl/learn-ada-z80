-- Example: bit_operations.adb
-- Concept: Bit manipulation for system programming
--
-- Demonstrates: modular types, bit operations.
-- Essential for Z80 hardware interfacing.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Bit_Operations is

   type Byte is mod 256;
   type Word is mod 65536;

   -- Print byte in binary
   procedure Print_Binary(B : Byte) is
      V : Byte := B;
   begin
      for I in reverse 0 .. 7 loop
         if (V and (2 ** I)) /= 0 then
            Put('1');
         else
            Put('0');
         end if;
      end loop;
   end Print_Binary;

   -- Set a specific bit
   function Set_Bit(B : Byte; Bit : Natural) return Byte is
   begin
      return B or (2 ** Bit);
   end Set_Bit;

   -- Clear a specific bit
   function Clear_Bit(B : Byte; Bit : Natural) return Byte is
   begin
      return B and (not (2 ** Bit));
   end Clear_Bit;

   -- Toggle a specific bit
   function Toggle_Bit(B : Byte; Bit : Natural) return Byte is
   begin
      return B xor (2 ** Bit);
   end Toggle_Bit;

   -- Test if bit is set
   function Is_Bit_Set(B : Byte; Bit : Natural) return Boolean is
   begin
      return (B and (2 ** Bit)) /= 0;
   end Is_Bit_Set;

   -- Extract bits (mask from low to high)
   function Extract_Bits(B : Byte; Low, High : Natural) return Byte is
      Mask : Byte := 0;
   begin
      for I in Low .. High loop
         Mask := Mask or (2 ** I);
      end loop;
      return (B and Mask) / (2 ** Low);
   end Extract_Bits;

   -- Rotate left
   function Rotate_Left(B : Byte; Count : Natural) return Byte is
      V : Byte := B;
   begin
      for I in 1 .. Count loop
         V := (V * 2) or (V / 128);
      end loop;
      return V;
   end Rotate_Left;

   -- Rotate right
   function Rotate_Right(B : Byte; Count : Natural) return Byte is
      V : Byte := B;
   begin
      for I in 1 .. Count loop
         V := (V / 2) or ((V and 1) * 128);
      end loop;
      return V;
   end Rotate_Right;

   B, Result : Byte;
begin
   Put_Line("=== Bit Operations ===");
   New_Line;

   -- Basic operations
   B := 16#A5#;  -- 10100101
   Put("Original:      ");
   Print_Binary(B);
   Put_Line(" (0xA5)");

   Put("AND 0x0F:      ");
   Print_Binary(B and 16#0F#);
   New_Line;

   Put("OR 0x0F:       ");
   Print_Binary(B or 16#0F#);
   New_Line;

   Put("XOR 0xFF:      ");
   Print_Binary(B xor 16#FF#);
   New_Line;

   Put("NOT:           ");
   Print_Binary(not B);
   New_Line;
   New_Line;

   -- Bit manipulation
   Put_Line("Bit manipulation on 0x00:");
   B := 0;
   B := Set_Bit(B, 0);
   Put("  Set bit 0:   ");
   Print_Binary(B);
   New_Line;

   B := Set_Bit(B, 4);
   Put("  Set bit 4:   ");
   Print_Binary(B);
   New_Line;

   B := Set_Bit(B, 7);
   Put("  Set bit 7:   ");
   Print_Binary(B);
   New_Line;

   B := Clear_Bit(B, 4);
   Put("  Clear bit 4: ");
   Print_Binary(B);
   New_Line;

   B := Toggle_Bit(B, 0);
   Put("  Toggle bit 0:");
   Print_Binary(B);
   New_Line;
   New_Line;

   -- Testing bits
   B := 16#A5#;
   Put_Line("Testing bits of 0xA5:");
   for I in 0 .. 7 loop
      Put("  Bit" & Natural'Image(I) & " = " &
          Boolean'Image(Is_Bit_Set(B, I)));
      New_Line;
   end loop;
   New_Line;

   -- Extract bits
   B := 16#B7#;  -- 10110111
   Put("Extract bits 2..5 from ");
   Print_Binary(B);
   Put(": ");
   Print_Binary(Extract_Bits(B, 2, 5));
   Put_Line(" (" & Byte'Image(Extract_Bits(B, 2, 5)) & ")");
   New_Line;

   -- Rotate
   B := 16#81#;  -- 10000001
   Put("Original:       ");
   Print_Binary(B);
   New_Line;
   Put("Rotate left 1:  ");
   Print_Binary(Rotate_Left(B, 1));
   New_Line;
   Put("Rotate right 1: ");
   Print_Binary(Rotate_Right(B, 1));
   New_Line;
end Bit_Operations;
