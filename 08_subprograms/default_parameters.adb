-- Example: default_parameters.adb
-- Concept: Default parameter values
--
-- Parameters can have default values.
-- Caller can omit parameters with defaults.
-- Useful for optional parameters.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Default_Parameters is

   -- Procedure with default parameter
   procedure Greet(Name : String := "World") is
   begin
      Put_Line("Hello, " & Name & "!");
   end Greet;

   -- Function with multiple defaults
   function Format_Number(Value   : Integer;
                          Width   : Positive := 1;
                          Fill    : Character := ' ') return String is
      Img : constant String := Integer'Image(Value);
      Len : constant Natural := Img'Length;
   begin
      if Len >= Width then
         return Img;
      else
         return (1 .. Width - Len => Fill) & Img;
      end if;
   end Format_Number;

   -- Procedure with some defaults
   procedure Draw_Box(Width  : Positive;
                      Height : Positive;
                      Char   : Character := '*') is
   begin
      for Row in 1 .. Height loop
         for Col in 1 .. Width loop
            Put(Char);
         end loop;
         New_Line;
      end loop;
   end Draw_Box;

   -- Only trailing parameters can have defaults
   procedure Log_Message(Level   : String := "INFO";
                         Message : String) is
   begin
      Put_Line("[" & Level & "] " & Message);
   end Log_Message;
begin
   Put_Line("Default parameter examples:");

   -- Using default
   Greet;  -- Uses default "World"

   -- Overriding default
   Greet("Ada");

   -- Format with defaults
   Put_Line("Format with defaults:");
   Put_Line("  Format_Number(42) = " & Format_Number(42));
   Put_Line("  Format_Number(42, 5) = " & Format_Number(42, 5));
   Put_Line("  Format_Number(42, 5, '0') = " & Format_Number(42, 5, '0'));

   -- Named parameters let you skip to later defaults
   Put_Line("  Format_Number(42, Fill => '#') = " &
            Format_Number(42, Fill => '#', Width => 5));

   -- Draw boxes
   Put_Line("Draw_Box(5, 2):");
   Draw_Box(5, 2);

   Put_Line("Draw_Box(5, 2, '+'):");
   Draw_Box(5, 2, '+');

   -- Named parameter association
   Put_Line("Named parameters:");
   Draw_Box(Height => 1, Width => 10, Char => '-');

   -- Log with default level
   Log_Message(Message => "System started");
   Log_Message("ERROR", "Something went wrong");
end Default_Parameters;
