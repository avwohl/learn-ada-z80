-- Example: morse_code.adb
-- Concept: Lookup table and string processing
--
-- Demonstrates: arrays, strings, case statement,
-- character processing. Fun retro application!

with Ada.Text_IO;
use Ada.Text_IO;

procedure Morse_Code is

   -- Morse code patterns (. = dit, - = dah)
   type Morse_Pattern is access constant String;

   Morse_A : aliased constant String := ".-";
   Morse_B : aliased constant String := "-...";
   Morse_C : aliased constant String := "-.-.";
   Morse_D : aliased constant String := "-..";
   Morse_E : aliased constant String := ".";
   Morse_F : aliased constant String := "..-.";
   Morse_G : aliased constant String := "--.";
   Morse_H : aliased constant String := "....";
   Morse_I : aliased constant String := "..";
   Morse_J : aliased constant String := ".---";
   Morse_K : aliased constant String := "-.-";
   Morse_L : aliased constant String := ".-..";
   Morse_M : aliased constant String := "--";
   Morse_N : aliased constant String := "-.";
   Morse_O : aliased constant String := "---";
   Morse_P : aliased constant String := ".--.";
   Morse_Q : aliased constant String := "--.-";
   Morse_R : aliased constant String := ".-.";
   Morse_S : aliased constant String := "...";
   Morse_T : aliased constant String := "-";
   Morse_U : aliased constant String := "..-";
   Morse_V : aliased constant String := "...-";
   Morse_W : aliased constant String := ".--";
   Morse_X : aliased constant String := "-..-";
   Morse_Y : aliased constant String := "-.--";
   Morse_Z : aliased constant String := "--..";

   function To_Upper(C : Character) return Character is
   begin
      if C in 'a' .. 'z' then
         return Character'Val(Character'Pos(C) - 32);
      else
         return C;
      end if;
   end To_Upper;

   function Char_To_Morse(C : Character) return String is
   begin
      case To_Upper(C) is
         when 'A' => return ".-";
         when 'B' => return "-...";
         when 'C' => return "-.-.";
         when 'D' => return "-..";
         when 'E' => return ".";
         when 'F' => return "..-.";
         when 'G' => return "--.";
         when 'H' => return "....";
         when 'I' => return "..";
         when 'J' => return ".---";
         when 'K' => return "-.-";
         when 'L' => return ".-..";
         when 'M' => return "--";
         when 'N' => return "-.";
         when 'O' => return "---";
         when 'P' => return ".--.";
         when 'Q' => return "--.-";
         when 'R' => return ".-.";
         when 'S' => return "...";
         when 'T' => return "-";
         when 'U' => return "..-";
         when 'V' => return "...-";
         when 'W' => return ".--";
         when 'X' => return "-..-";
         when 'Y' => return "-.--";
         when 'Z' => return "--..";
         when '0' => return "-----";
         when '1' => return ".----";
         when '2' => return "..---";
         when '3' => return "...--";
         when '4' => return "....-";
         when '5' => return ".....";
         when '6' => return "-....";
         when '7' => return "--...";
         when '8' => return "---..";
         when '9' => return "----.";
         when ' ' => return "/";  -- Word separator
         when others => return "?";
      end case;
   end Char_To_Morse;

   procedure Encode(Text : String) is
   begin
      Put("Text: " & Text);
      New_Line;
      Put("Morse: ");
      for C of Text loop
         Put(Char_To_Morse(C) & " ");
      end loop;
      New_Line;
   end Encode;

begin
   Put_Line("=== Morse Code Encoder ===");
   New_Line;

   Put_Line("Alphabet:");
   for C in 'A' .. 'Z' loop
      Put("  " & C & " = " & Char_To_Morse(C));
      if (Character'Pos(C) - Character'Pos('A') + 1) mod 5 = 0 then
         New_Line;
      end if;
   end loop;
   New_Line;
   New_Line;

   Put_Line("Numbers:");
   for C in '0' .. '9' loop
      Put("  " & C & " = " & Char_To_Morse(C));
   end loop;
   New_Line;
   New_Line;

   Put_Line("Example messages:");
   New_Line;
   Encode("HELLO");
   New_Line;
   Encode("SOS");
   New_Line;
   Encode("ADA");
   New_Line;
   Encode("Z80 CPM");
end Morse_Code;
