-- Example: generic_function.adb
-- Concept: Generic functions
--
-- Generic functions can work with multiple types.
-- Formal parameters specify requirements on types.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Generic_Function is

   -- Generic max function for any comparable type
   generic
      type Element_Type is private;
      with function "<"(Left, Right : Element_Type) return Boolean is <>;
   function Generic_Max(A, B : Element_Type) return Element_Type;

   function Generic_Max(A, B : Element_Type) return Element_Type is
   begin
      if A < B then
         return B;
      else
         return A;
      end if;
   end Generic_Max;

   -- Instantiate for different types
   function Max_Int is new Generic_Max(Integer);
   function Max_Char is new Generic_Max(Character);

   -- Generic array sum
   generic
      type Element_Type is private;
      type Index_Type is (<>);
      type Array_Type is array(Index_Type range <>) of Element_Type;
      Zero : Element_Type;
      with function "+"(Left, Right : Element_Type) return Element_Type is <>;
   function Generic_Sum(A : Array_Type) return Element_Type;

   function Generic_Sum(A : Array_Type) return Element_Type is
      Result : Element_Type := Zero;
   begin
      for I in A'Range loop
         Result := Result + A(I);
      end loop;
      return Result;
   end Generic_Sum;

   type Int_Array is array(Positive range <>) of Integer;
   function Sum_Ints is new Generic_Sum(Integer, Positive, Int_Array, 0);

   -- Generic identity function
   generic
      type T is private;
   function Identity(X : T) return T;

   function Identity(X : T) return T is
   begin
      return X;
   end Identity;

   function Int_Identity is new Identity(Integer);
   function Str_Identity is new Identity(String);

   Arr : Int_Array := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
begin
   Put_Line("Generic function examples:");

   -- Max function
   Put_Line("Max_Int(10, 20) =" & Integer'Image(Max_Int(10, 20)));
   Put_Line("Max_Int(30, 15) =" & Integer'Image(Max_Int(30, 15)));
   Put_Line("Max_Char('A', 'Z') = " & Max_Char('A', 'Z'));

   -- Sum function
   Put_Line("Sum of 1..10 =" & Integer'Image(Sum_Ints(Arr)));

   -- Identity function
   Put_Line("Int_Identity(42) =" & Integer'Image(Int_Identity(42)));
   Put_Line("Str_Identity(""Hello"") = " & Str_Identity("Hello"));
end Generic_Function;
