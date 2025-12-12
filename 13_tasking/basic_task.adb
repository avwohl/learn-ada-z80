-- Example: basic_task.adb
-- Concept: Basic tasking (concurrent execution)
--
-- Tasks are Ada's built-in concurrency mechanism.
-- Tasks run concurrently with the main program.
-- Each task has its own thread of control.
--
-- Note: On Z80/CP/M, tasking requires runtime support
-- (typically via timer interrupts for preemption)

with Ada.Text_IO;
use Ada.Text_IO;

procedure Basic_Task is

   -- Declare a simple task
   task Simple_Task;

   -- Task body
   task body Simple_Task is
   begin
      Put_Line("Simple_Task: Starting");
      for I in 1 .. 3 loop
         Put_Line("Simple_Task: Count" & Integer'Image(I));
      end loop;
      Put_Line("Simple_Task: Finished");
   end Simple_Task;

   -- Task with loop
   task Counter_Task;

   task body Counter_Task is
   begin
      Put_Line("Counter_Task: Starting");
      for I in reverse 1 .. 5 loop
         Put_Line("Counter_Task: Countdown" & Integer'Image(I));
      end loop;
      Put_Line("Counter_Task: Blastoff!");
   end Counter_Task;

begin
   Put_Line("Main: Program starting");
   Put_Line("Main: Tasks are running concurrently...");

   -- Tasks start automatically when we reach this point
   -- Main program continues while tasks run

   for I in 1 .. 3 loop
      Put_Line("Main: Working" & Integer'Image(I));
   end loop;

   Put_Line("Main: Waiting for tasks to complete...");
   -- Main implicitly waits for all tasks to finish

   -- Note: Order of output lines may vary due to concurrency!
   Put_Line("Main: All tasks completed, program ending");
end Basic_Task;
