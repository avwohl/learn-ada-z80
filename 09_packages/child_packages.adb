-- Example: child_packages.adb
-- Concept: Child packages (hierarchical packages)
--
-- Child packages extend parent packages.
-- They have access to the parent's private part.
-- Naming: Parent.Child

with Ada.Text_IO;
use Ada.Text_IO;

procedure Child_Packages is

   -- Parent package
   package Parent is
      procedure Hello;
      Value : Integer := 100;
   private
      Secret : Integer := 42;  -- Only visible to children
   end Parent;

   package body Parent is
      procedure Hello is
      begin
         Put_Line("Hello from Parent!");
      end Hello;
   end Parent;

   -- Child package (Parent.Child)
   package Parent.Child is
      procedure Hello;
      procedure Show_Secret;  -- Can access Parent's private part
   end Parent.Child;

   package body Parent.Child is
      procedure Hello is
      begin
         Put_Line("Hello from Child!");
         Put_Line("  Parent.Value =" & Integer'Image(Parent.Value));
      end Hello;

      procedure Show_Secret is
      begin
         -- Child can see parent's private part!
         Put_Line("  Parent.Secret =" & Integer'Image(Parent.Secret));
      end Show_Secret;
   end Parent.Child;

   -- Grandchild package
   package Parent.Child.Grandchild is
      procedure Hello;
   end Parent.Child.Grandchild;

   package body Parent.Child.Grandchild is
      procedure Hello is
      begin
         Put_Line("Hello from Grandchild!");
      end Hello;
   end Parent.Child.Grandchild;

begin
   Put_Line("Child package examples:");

   -- Use parent
   Parent.Hello;
   Put_Line("Parent.Value =" & Integer'Image(Parent.Value));

   -- Parent.Secret not accessible here (it's private)
   -- Put_Line(Integer'Image(Parent.Secret));  -- Error!

   -- Use child
   Parent.Child.Hello;
   Parent.Child.Show_Secret;  -- Child CAN access parent's secret

   -- Use grandchild
   Parent.Child.Grandchild.Hello;

   -- With "use" clause
   declare
      use Parent.Child;
   begin
      Hello;  -- Calls Parent.Child.Hello
   end;
end Child_Packages;
