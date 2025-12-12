-- Example: hello_use.adb
-- Concept: Using the "use" clause to avoid package prefixes
--
-- The "use" clause makes all names from a package directly visible
-- Without it, you must write Ada.Text_IO.Put_Line
-- With it, you can just write Put_Line

with Ada.Text_IO;
use Ada.Text_IO;  -- Makes Put_Line directly visible

procedure Hello_Use is
begin
   Put_Line("Hello with use clause!");
   Put("No newline here...");
   New_Line;  -- Outputs a newline character
   Put_Line("...but there is one after this.");
end Hello_Use;
