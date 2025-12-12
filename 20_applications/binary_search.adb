-- Example: binary_search.adb
-- Concept: Binary search algorithm
--
-- Demonstrates: recursion, functions, arrays.
-- Efficient O(log n) search for sorted data.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Binary_Search is

   type Int_Array is array(Positive range <>) of Integer;

   -- Iterative binary search
   function Search_Iterative(A : Int_Array; Target : Integer) return Natural is
      Low  : Integer := A'First;
      High : Integer := A'Last;
      Mid  : Integer;
   begin
      while Low <= High loop
         Mid := (Low + High) / 2;
         if A(Mid) = Target then
            return Mid;
         elsif A(Mid) < Target then
            Low := Mid + 1;
         else
            High := Mid - 1;
         end if;
      end loop;
      return 0;  -- Not found
   end Search_Iterative;

   -- Recursive binary search
   function Search_Recursive(A : Int_Array;
                             Target : Integer;
                             Low, High : Integer) return Natural is
      Mid : Integer;
   begin
      if Low > High then
         return 0;  -- Not found
      end if;

      Mid := (Low + High) / 2;

      if A(Mid) = Target then
         return Mid;
      elsif A(Mid) < Target then
         return Search_Recursive(A, Target, Mid + 1, High);
      else
         return Search_Recursive(A, Target, Low, Mid - 1);
      end if;
   end Search_Recursive;

   -- Wrapper for recursive search
   function Search(A : Int_Array; Target : Integer) return Natural is
   begin
      return Search_Recursive(A, Target, A'First, A'Last);
   end Search;

   -- Find insertion point (where to insert to keep sorted)
   function Find_Insert_Point(A : Int_Array; Target : Integer) return Positive is
      Low  : Integer := A'First;
      High : Integer := A'Last;
      Mid  : Integer;
   begin
      while Low <= High loop
         Mid := (Low + High) / 2;
         if A(Mid) < Target then
            Low := Mid + 1;
         else
            High := Mid - 1;
         end if;
      end loop;
      return Low;
   end Find_Insert_Point;

   -- Test data (must be sorted!)
   Data : constant Int_Array := (10, 20, 30, 40, 50, 60, 70, 80, 90, 100);
   Idx : Natural;
begin
   Put_Line("=== Binary Search Examples ===");
   New_Line;

   Put("Sorted array:");
   for V of Data loop
      Put(Integer'Image(V));
   end loop;
   New_Line;
   New_Line;

   -- Search for existing values
   Put_Line("Searching for existing values:");
   for Target of Int_Array'(10, 50, 100) loop
      Idx := Search_Iterative(Data, Target);
      Put_Line("  " & Integer'Image(Target) &
               " found at index" & Natural'Image(Idx));
   end loop;
   New_Line;

   -- Search for non-existing values
   Put_Line("Searching for non-existing values:");
   for Target of Int_Array'(15, 55, 105) loop
      Idx := Search(Data, Target);
      if Idx = 0 then
         Put_Line("  " & Integer'Image(Target) & " not found");
      else
         Put_Line("  " & Integer'Image(Target) &
                  " found at index" & Natural'Image(Idx));
      end if;
   end loop;
   New_Line;

   -- Find insertion points
   Put_Line("Insertion points:");
   for Target of Int_Array'(5, 35, 105) loop
      Idx := Find_Insert_Point(Data, Target);
      Put_Line("  Insert" & Integer'Image(Target) &
               " at position" & Natural'Image(Idx));
   end loop;
end Binary_Search;
