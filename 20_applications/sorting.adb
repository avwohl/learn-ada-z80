-- Example: sorting.adb
-- Concept: Sorting algorithms
--
-- Demonstrates: arrays, procedures, generics.
-- Common algorithms useful on Z80.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Sorting is

   type Int_Array is array(Positive range <>) of Integer;

   -- Bubble sort (simple but O(n^2))
   procedure Bubble_Sort(A : in Out Int_Array) is
      Temp : Integer;
      Swapped : Boolean;
   begin
      loop
         Swapped := False;
         for I in A'First .. A'Last - 1 loop
            if A(I) > A(I + 1) then
               Temp := A(I);
               A(I) := A(I + 1);
               A(I + 1) := Temp;
               Swapped := True;
            end if;
         end loop;
         exit when not Swapped;
      end loop;
   end Bubble_Sort;

   -- Selection sort
   procedure Selection_Sort(A : in Out Int_Array) is
      Min_Idx : Positive;
      Temp : Integer;
   begin
      for I in A'First .. A'Last - 1 loop
         Min_Idx := I;
         for J in I + 1 .. A'Last loop
            if A(J) < A(Min_Idx) then
               Min_Idx := J;
            end if;
         end loop;
         if Min_Idx /= I then
            Temp := A(I);
            A(I) := A(Min_Idx);
            A(Min_Idx) := Temp;
         end if;
      end loop;
   end Selection_Sort;

   -- Insertion sort (good for nearly sorted data)
   procedure Insertion_Sort(A : in Out Int_Array) is
      Key : Integer;
      J : Integer;
   begin
      for I in A'First + 1 .. A'Last loop
         Key := A(I);
         J := I - 1;
         while J >= A'First and then A(J) > Key loop
            A(J + 1) := A(J);
            J := J - 1;
         end loop;
         A(J + 1) := Key;
      end loop;
   end Insertion_Sort;

   procedure Print_Array(Name : String; A : Int_Array) is
   begin
      Put(Name & ":");
      for I in A'Range loop
         Put(Integer'Image(A(I)));
      end loop;
      New_Line;
   end Print_Array;

   Data1 : Int_Array := (64, 34, 25, 12, 22, 11, 90);
   Data2 : Int_Array := (64, 34, 25, 12, 22, 11, 90);
   Data3 : Int_Array := (64, 34, 25, 12, 22, 11, 90);
begin
   Put_Line("=== Sorting Algorithms ===");
   New_Line;

   -- Bubble Sort
   Put_Line("Bubble Sort:");
   Print_Array("  Before", Data1);
   Bubble_Sort(Data1);
   Print_Array("  After ", Data1);
   New_Line;

   -- Selection Sort
   Put_Line("Selection Sort:");
   Print_Array("  Before", Data2);
   Selection_Sort(Data2);
   Print_Array("  After ", Data2);
   New_Line;

   -- Insertion Sort
   Put_Line("Insertion Sort:");
   Print_Array("  Before", Data3);
   Insertion_Sort(Data3);
   Print_Array("  After ", Data3);
   New_Line;

   -- Verify all produce same result
   Put_Line("Verification:");
   if Data1 = Data2 and Data2 = Data3 then
      Put_Line("  All algorithms produced the same sorted array!");
   else
      Put_Line("  Warning: Results differ!");
   end if;
end Sorting;
