-- Example: character_type.adb
-- Concept: Character type in Ada
--
-- Character is a built-in enumeration type covering
-- the 256 values of Latin-1 (ASCII + extended).
-- Character literals are enclosed in single quotes: 'A'

with Ada.Text_IO;
use Ada.Text_IO;

procedure Character_Type is
   Letter    : Character := 'A';
   Digit     : Character := '5';
   Space     : Character := ' ';
   Newline   : Character := ASCII.LF;  -- Line feed
   Tab       : Character := ASCII.HT;  -- Horizontal tab

   C : Character;
begin
   Put_Line("Character examples:");
   Put("  Letter: ");
   Put(Letter);
   New_Line;

   Put("  Digit: ");
   Put(Digit);
   New_Line;

   -- Character position (ordinal value)
   Put_Line("Character positions (ASCII values):");
   Put_Line("  'A' position:" & Integer'Image(Character'Pos('A')));
   Put_Line("  'Z' position:" & Integer'Image(Character'Pos('Z')));
   Put_Line("  'a' position:" & Integer'Image(Character'Pos('a')));
   Put_Line("  '0' position:" & Integer'Image(Character'Pos('0')));

   -- Character from position
   C := Character'Val(66);  -- 'B'
   Put("  Character at position 66: ");
   Put(C);
   New_Line;

   -- Character comparisons
   Put_Line("Character comparisons:");
   Put_Line("  'A' < 'B' is " & Boolean'Image('A' < 'B'));
   Put_Line("  'a' > 'A' is " & Boolean'Image('a' > 'A'));

   -- Character range
   Put_Line("Iterating 'A' to 'E':");
   for Ch in 'A' .. 'E' loop
      Put("  ");
      Put(Ch);
      New_Line;
   end loop;
end Character_Type;
