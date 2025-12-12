-- Example: todo_list.adb
-- Concept: Simple task manager with records and arrays
--
-- Demonstrates: records, arrays, enumerations,
-- procedures, and data management.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Todo_List is

   Max_Tasks : constant := 10;
   Max_Desc_Len : constant := 40;

   type Priority is (Low, Medium, High, Urgent);
   type Status is (Pending, In_Progress, Done);

   type Task_Item is record
      Description : String(1 .. Max_Desc_Len);
      Desc_Length : Natural := 0;
      Prio        : Priority := Medium;
      State       : Status := Pending;
      Active      : Boolean := False;
   end record;

   type Task_List is array(1 .. Max_Tasks) of Task_Item;

   Tasks : Task_List;
   Task_Count : Natural := 0;

   procedure Add_Task(Desc : String; P : Priority := Medium) is
   begin
      if Task_Count >= Max_Tasks then
         Put_Line("Error: Task list full!");
         return;
      end if;

      Task_Count := Task_Count + 1;

      -- Copy description (truncate if too long)
      declare
         Len : constant Natural := Integer'Min(Desc'Length, Max_Desc_Len);
      begin
         Tasks(Task_Count).Description := (others => ' ');
         Tasks(Task_Count).Description(1 .. Len) := Desc(Desc'First .. Desc'First + Len - 1);
         Tasks(Task_Count).Desc_Length := Len;
      end;

      Tasks(Task_Count).Prio := P;
      Tasks(Task_Count).State := Pending;
      Tasks(Task_Count).Active := True;
   end Add_Task;

   procedure Set_Status(Index : Positive; S : Status) is
   begin
      if Index <= Task_Count and then Tasks(Index).Active then
         Tasks(Index).State := S;
      end if;
   end Set_Status;

   procedure Print_Task(Index : Positive) is
      T : Task_Item renames Tasks(Index);
      State_Char : Character;
   begin
      if not T.Active then
         return;
      end if;

      case T.State is
         when Pending     => State_Char := ' ';
         when In_Progress => State_Char := '*';
         when Done        => State_Char := 'X';
      end case;

      Put("[" & State_Char & "] ");
      Put(Integer'Image(Index) & ". ");
      Put(T.Description(1 .. T.Desc_Length));
      Set_Col(50);
      Put("[" & Priority'Image(T.Prio) & "]");
      New_Line;
   end Print_Task;

   procedure Print_All_Tasks is
   begin
      Put_Line("=== Task List ===");
      Put_Line("[ ] = Pending, [*] = In Progress, [X] = Done");
      New_Line;

      if Task_Count = 0 then
         Put_Line("No tasks!");
         return;
      end if;

      for I in 1 .. Task_Count loop
         Print_Task(I);
      end loop;
   end Print_All_Tasks;

   procedure Count_By_Status is
      Pending_Count : Natural := 0;
      Progress_Count : Natural := 0;
      Done_Count : Natural := 0;
   begin
      for I in 1 .. Task_Count loop
         if Tasks(I).Active then
            case Tasks(I).State is
               when Pending => Pending_Count := Pending_Count + 1;
               when In_Progress => Progress_Count := Progress_Count + 1;
               when Done => Done_Count := Done_Count + 1;
            end case;
         end if;
      end loop;

      Put_Line("Summary:");
      Put_Line("  Pending:" & Natural'Image(Pending_Count));
      Put_Line("  In Progress:" & Natural'Image(Progress_Count));
      Put_Line("  Done:" & Natural'Image(Done_Count));
   end Count_By_Status;

begin
   Put_Line("=== Todo List Application ===");
   New_Line;

   -- Add some tasks
   Add_Task("Write Ada examples", High);
   Add_Task("Test on Z80 emulator", High);
   Add_Task("Update documentation", Medium);
   Add_Task("Fix parser bugs", Urgent);
   Add_Task("Add more test cases", Low);

   Print_All_Tasks;
   New_Line;

   -- Update some statuses
   Put_Line("Updating task statuses...");
   Set_Status(1, Done);
   Set_Status(2, In_Progress);
   Set_Status(4, In_Progress);
   New_Line;

   Print_All_Tasks;
   New_Line;

   Count_By_Status;
end Todo_List;
