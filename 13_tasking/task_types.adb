-- Example: task_types.adb
-- Concept: Task types (creating multiple tasks)
--
-- Task types let you create multiple instances of a task.
-- Each instance runs independently.
-- Can pass data to tasks via discriminants.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Task_Types is

   -- Task type with discriminant
   task type Worker(ID : Positive);

   task body Worker is
   begin
      Put_Line("Worker" & Positive'Image(ID) & ": Starting");
      for I in 1 .. 3 loop
         Put_Line("Worker" & Positive'Image(ID) &
                  ": Step" & Integer'Image(I));
      end loop;
      Put_Line("Worker" & Positive'Image(ID) & ": Done");
   end Worker;

   -- Task type without discriminant
   task type Printer;

   task body Printer is
   begin
      Put_Line("Printer task running");
   end Printer;

   -- Create task instances
   W1 : Worker(1);
   W2 : Worker(2);
   W3 : Worker(3);

   P1, P2 : Printer;

begin
   Put_Line("Main: Created 3 workers and 2 printers");
   Put_Line("Main: Tasks are executing...");

   -- All tasks run concurrently
   -- Order of output is non-deterministic

   Put_Line("Main: Waiting for all tasks...");
   -- Implicit wait for all tasks at end of scope
end Task_Types;
