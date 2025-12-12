-- Example: use_stack.adb
-- Concept: Using a package
--
-- This demonstrates using the Stack_Pkg package.
-- Shows "with" for visibility and "use" for direct naming.

with Ada.Text_IO;
with Stack_Pkg;    -- Make Stack_Pkg visible

use Ada.Text_IO;
-- Note: NOT using "use Stack_Pkg" - using qualified names instead

procedure Use_Stack is
   My_Stack : Stack_Pkg.Stack;  -- Qualified name for type
   Value    : Integer;
begin
   Put_Line("Using the Stack package:");

   -- Check initial state
   Put_Line("Is_Empty: " & Boolean'Image(Stack_Pkg.Is_Empty(My_Stack)));
   Put_Line("Size:" & Natural'Image(Stack_Pkg.Size(My_Stack)));

   -- Push some values
   Put_Line("Pushing 10, 20, 30:");
   Stack_Pkg.Push(My_Stack, 10);
   Stack_Pkg.Push(My_Stack, 20);
   Stack_Pkg.Push(My_Stack, 30);

   Put_Line("Size after pushes:" & Natural'Image(Stack_Pkg.Size(My_Stack)));
   Put_Line("Top:" & Integer'Image(Stack_Pkg.Top(My_Stack)));

   -- Pop values
   Put_Line("Popping:");
   while not Stack_Pkg.Is_Empty(My_Stack) loop
      Stack_Pkg.Pop(My_Stack, Value);
      Put_Line("  Popped:" & Integer'Image(Value));
   end loop;

   -- Try to pop from empty stack (exception)
   Put_Line("Attempting to pop from empty stack:");
   begin
      Stack_Pkg.Pop(My_Stack, Value);
   exception
      when Stack_Pkg.Stack_Empty =>
         Put_Line("  Caught Stack_Empty exception!");
   end;

   -- Alternative with "use" (uncomment to use)
   -- use Stack_Pkg;
   -- Push(My_Stack, 100);  -- No need for Stack_Pkg. prefix
end Use_Stack;
