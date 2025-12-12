-- Example: generic_package.adb
-- Concept: Generic packages
--
-- Generic packages encapsulate types and operations.
-- Most powerful form of genericity in Ada.
-- Instantiate to create concrete packages.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Generic_Package is

   -- Generic stack package
   generic
      type Element_Type is private;
      Max_Size : Positive := 100;
   package Generic_Stack is
      type Stack is private;

      Stack_Empty : exception;
      Stack_Full  : exception;

      procedure Push(S : in Out Stack; Item : Element_Type);
      procedure Pop(S : in Out Stack; Item : out Element_Type);
      function Top(S : Stack) return Element_Type;
      function Is_Empty(S : Stack) return Boolean;
      function Size(S : Stack) return Natural;
      procedure Clear(S : in Out Stack);

   private
      type Item_Array is array(1 .. Max_Size) of Element_Type;
      type Stack is record
         Items : Item_Array;
         Count : Natural := 0;
      end record;
   end Generic_Stack;

   package body Generic_Stack is
      procedure Push(S : in Out Stack; Item : Element_Type) is
      begin
         if S.Count >= Max_Size then
            raise Stack_Full;
         end if;
         S.Count := S.Count + 1;
         S.Items(S.Count) := Item;
      end Push;

      procedure Pop(S : in Out Stack; Item : out Element_Type) is
      begin
         if S.Count = 0 then
            raise Stack_Empty;
         end if;
         Item := S.Items(S.Count);
         S.Count := S.Count - 1;
      end Pop;

      function Top(S : Stack) return Element_Type is
      begin
         if S.Count = 0 then
            raise Stack_Empty;
         end if;
         return S.Items(S.Count);
      end Top;

      function Is_Empty(S : Stack) return Boolean is
      begin
         return S.Count = 0;
      end Is_Empty;

      function Size(S : Stack) return Natural is
      begin
         return S.Count;
      end Size;

      procedure Clear(S : in Out Stack) is
      begin
         S.Count := 0;
      end Clear;
   end Generic_Stack;

   -- Instantiate for Integer
   package Int_Stack is new Generic_Stack(Integer, 10);

   -- Instantiate for Character
   package Char_Stack is new Generic_Stack(Character, 20);

   S1 : Int_Stack.Stack;
   S2 : Char_Stack.Stack;
   V  : Integer;
   C  : Character;
begin
   Put_Line("Generic package examples:");

   -- Use integer stack
   Put_Line("Integer stack:");
   Int_Stack.Push(S1, 10);
   Int_Stack.Push(S1, 20);
   Int_Stack.Push(S1, 30);
   Put_Line("  Size:" & Natural'Image(Int_Stack.Size(S1)));
   Put_Line("  Top:" & Integer'Image(Int_Stack.Top(S1)));

   Put("  Popping:");
   while not Int_Stack.Is_Empty(S1) loop
      Int_Stack.Pop(S1, V);
      Put(Integer'Image(V));
   end loop;
   New_Line;

   -- Use character stack
   Put_Line("Character stack:");
   for Ch in Character range 'A' .. 'E' loop
      Char_Stack.Push(S2, Ch);
   end loop;
   Put("  Reversed: ");
   while not Char_Stack.Is_Empty(S2) loop
      Char_Stack.Pop(S2, C);
      Put(C);
   end loop;
   New_Line;
end Generic_Package;
