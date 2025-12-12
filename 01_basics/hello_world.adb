-- Example: hello_world.adb
-- Concept: The simplest Ada program - printing text to console
--
-- Every Ada program needs:
--   1. A "with" clause to import packages (like #include in C)
--   2. A procedure as the main entry point
--   3. The procedure body between "begin" and "end"

with Ada.Text_IO;  -- Import the Text I/O package

procedure Hello_World is
   -- Declarations would go here (between "is" and "begin")
begin
   Ada.Text_IO.Put_Line("Hello, World!");
end Hello_World;
