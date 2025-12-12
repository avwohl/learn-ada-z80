-- Example: text_io_basics.adb
-- Concept: Basic text I/O operations
--
-- Ada.Text_IO provides console and file I/O for text.
-- Put/Get for characters and strings
-- Put_Line/Get_Line for lines
-- New_Line for line breaks

with Ada.Text_IO;
use Ada.Text_IO;

procedure Text_IO_Basics is
   Line : String(1 .. 80);
   Last : Natural;
   C    : Character;
begin
   Put_Line("Text I/O basics:");

   -- Put_Line: output string with newline
   Put_Line("This is a complete line.");

   -- Put: output without newline
   Put("First part... ");
   Put("Second part... ");
   Put("Third part.");
   New_Line;  -- Now add newline

   -- Multiple New_Lines
   Put_Line("Before blank lines");
   New_Line(2);  -- Two blank lines
   Put_Line("After blank lines");

   -- Output characters
   Put("Characters: ");
   Put('A');
   Put('B');
   Put('C');
   New_Line;

   -- Character by character in loop
   Put("Loop output: ");
   for C in 'A' .. 'E' loop
      Put(C);
      Put(' ');
   end loop;
   New_Line;

   -- Set_Col for positioning
   Put_Line("Column positioning:");
   Put("Name");
   Set_Col(20);
   Put("Age");
   Set_Col(30);
   Put_Line("City");
   Put("Alice");
   Set_Col(20);
   Put("25");
   Set_Col(30);
   Put_Line("Boston");

   -- Note: Get_Line reads from console (not demonstrated in output)
   -- Get_Line(Line, Last);  -- Reads line into Line, Last = actual length

   Put_Line("Text I/O demonstration complete.");
end Text_IO_Basics;
