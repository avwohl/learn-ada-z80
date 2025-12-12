-- Example: comments.adb
-- Concept: Ada comments
--
-- Ada only has single-line comments starting with --
-- There are no multi-line /* */ comments like in C
-- Comments extend from -- to end of line

with Ada.Text_IO;
use Ada.Text_IO;

procedure Comments is
   -- This is a comment explaining the variable below
   Message : constant String := "Comments are important!";

   -- Comments can appear on their own line
   X : Integer := 42;  -- Or at the end of a line
begin
   -- Comments in the executable part
   Put_Line(Message);

   -- You can "comment out" code:
   -- Put_Line("This line won't execute");

   Put_Line("X =" & Integer'Image(X));  -- Print the value
end Comments;
