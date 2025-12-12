-- Example: generic_formal_params.adb
-- Concept: Generic formal parameter kinds
--
-- Ada generics support many kinds of formal parameters:
-- - Types (private, limited, discrete, etc.)
-- - Objects (values, variables)
-- - Subprograms (functions, procedures)
-- - Packages

with Ada.Text_IO;
use Ada.Text_IO;

procedure Generic_Formal_Params is

   -- Formal type: private (any type with assignment and =)
   generic
      type T is private;
   procedure Demo_Private(X : T);

   procedure Demo_Private(X : T) is
      Copy : T := X;  -- Assignment works
   begin
      if Copy = X then  -- Equality works
         Put_Line("  Private type: copy equals original");
      end if;
   end Demo_Private;

   -- Formal type: limited private (no assignment or =)
   -- generic
   --    type T is limited private;
   -- procedure Demo_Limited(X : in T);  -- Can only pass in

   -- Formal type: discrete (integer or enumeration)
   generic
      type T is (<>);
   procedure Demo_Discrete;

   procedure Demo_Discrete is
   begin
      Put_Line("  First: " & T'Image(T'First));
      Put_Line("  Last: " & T'Image(T'Last));
   end Demo_Discrete;

   -- Formal type: integer type
   generic
      type T is range <>;
   function Demo_Integer(X : T) return T;

   function Demo_Integer(X : T) return T is
   begin
      return X * 2;  -- Arithmetic works
   end Demo_Integer;

   -- Formal object (value)
   generic
      Default_Value : Integer;
   function With_Default return Integer;

   function With_Default return Integer is
   begin
      return Default_Value;
   end With_Default;

   -- Formal subprogram
   generic
      type Element is private;
      with function Process(X : Element) return Element;
   function Apply_Twice(X : Element) return Element;

   function Apply_Twice(X : Element) return Element is
   begin
      return Process(Process(X));
   end Apply_Twice;

   -- Helper functions for Apply_Twice
   function Double(X : Integer) return Integer is (X * 2);
   function Inc(X : Integer) return Integer is (X + 1);

   -- Instantiations
   procedure Demo_Int is new Demo_Private(Integer);
   type Color is (Red, Green, Blue);
   procedure Demo_Color is new Demo_Discrete(Color);
   function Double_Small is new Demo_Integer(Integer);
   function Get_100 is new With_Default(100);
   function Get_42 is new With_Default(42);
   function Quadruple is new Apply_Twice(Integer, Double);
   function Add_Two is new Apply_Twice(Integer, Inc);

begin
   Put_Line("Generic formal parameters:");

   Put_Line("Private type formal:");
   Demo_Int(42);

   Put_Line("Discrete type formal (Color):");
   Demo_Color;

   Put_Line("Integer type formal:");
   Put_Line("  Double_Small(5) =" & Integer'Image(Double_Small(5)));

   Put_Line("Formal object:");
   Put_Line("  Get_100 =" & Integer'Image(Get_100));
   Put_Line("  Get_42 =" & Integer'Image(Get_42));

   Put_Line("Formal subprogram:");
   Put_Line("  Quadruple(3) = Double(Double(3)) =" &
            Integer'Image(Quadruple(3)));
   Put_Line("  Add_Two(10) = Inc(Inc(10)) =" &
            Integer'Image(Add_Two(10)));
end Generic_Formal_Params;
