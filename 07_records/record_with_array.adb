-- Example: record_with_array.adb
-- Concept: Records containing arrays
--
-- Records can contain array fields.
-- Useful for grouping an array with its metadata.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Record_With_Array is
   Max_Items : constant := 10;

   type Item_List is record
      Items : array(1 .. Max_Items) of Integer;
      Count : Natural := 0;  -- Number of valid items
   end record;

   type Student_Grades is record
      Name   : String(1 .. 20);
      Grades : array(1 .. 5) of Integer;
      Count  : Natural := 0;
   end record;

   List : Item_List;
   Student : Student_Grades;

   procedure Add_Item(L : in out Item_List; Value : Integer) is
   begin
      if L.Count < Max_Items then
         L.Count := L.Count + 1;
         L.Items(L.Count) := Value;
      end if;
   end Add_Item;

   procedure Print_List(L : Item_List) is
   begin
      Put("  Items (" & Natural'Image(L.Count) & "):");
      for I in 1 .. L.Count loop
         Put(Integer'Image(L.Items(I)));
      end loop;
      New_Line;
   end Print_List;

   function Average(S : Student_Grades) return Integer is
      Sum : Integer := 0;
   begin
      if S.Count = 0 then
         return 0;
      end if;
      for I in 1 .. S.Count loop
         Sum := Sum + S.Grades(I);
      end loop;
      return Sum / S.Count;
   end Average;
begin
   Put_Line("Record with array examples:");

   -- Initialize and add items
   List := (Items => (others => 0), Count => 0);
   Add_Item(List, 10);
   Add_Item(List, 20);
   Add_Item(List, 30);

   Put_Line("Item list:");
   Print_List(List);

   -- Student grades
   Student := (Name   => "Bob Smith           ",
               Grades => (85, 90, 78, 92, 88),
               Count  => 5);

   Put_Line("Student: " & Student.Name);
   Put("  Grades:");
   for I in 1 .. Student.Count loop
      Put(Integer'Image(Student.Grades(I)));
   end loop;
   New_Line;
   Put_Line("  Average:" & Integer'Image(Average(Student)));

   -- Access specific grade
   Put_Line("  First grade:" & Integer'Image(Student.Grades(1)));

   -- Modify a grade
   Student.Grades(3) := 82;
   Put_Line("  After changing grade 3 to 82:");
   Put_Line("  New average:" & Integer'Image(Average(Student)));
end Record_With_Array;
