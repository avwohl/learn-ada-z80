-- Example: linked_list.adb
-- Concept: Building data structures with access types
--
-- Access types enable dynamic data structures like linked lists.
-- Use incomplete type declaration for self-referential types.

with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
use Ada.Text_IO;

procedure Linked_List is

   -- Forward declaration (incomplete type)
   type Node;

   -- Access type for the node
   type Node_Ptr is access Node;

   -- Complete type definition
   type Node is record
      Data : Integer;
      Next : Node_Ptr := null;
   end record;

   -- Procedure to free memory
   procedure Free is new Ada.Unchecked_Deallocation(Node, Node_Ptr);

   Head : Node_Ptr := null;

   -- Insert at front
   procedure Push_Front(Value : Integer) is
      New_Node : Node_Ptr;
   begin
      New_Node := new Node'(Data => Value, Next => Head);
      Head := New_Node;
   end Push_Front;

   -- Insert at back
   procedure Push_Back(Value : Integer) is
      New_Node : Node_Ptr;
      Current  : Node_Ptr;
   begin
      New_Node := new Node'(Data => Value, Next => null);
      if Head = null then
         Head := New_Node;
      else
         Current := Head;
         while Current.Next /= null loop
            Current := Current.Next;
         end loop;
         Current.Next := New_Node;
      end if;
   end Push_Back;

   -- Print the list
   procedure Print_List is
      Current : Node_Ptr := Head;
   begin
      Put("List: ");
      while Current /= null loop
         Put(Integer'Image(Current.Data));
         if Current.Next /= null then
            Put(" ->");
         end if;
         Current := Current.Next;
      end loop;
      New_Line;
   end Print_List;

   -- Get list length
   function Length return Natural is
      Count   : Natural := 0;
      Current : Node_Ptr := Head;
   begin
      while Current /= null loop
         Count := Count + 1;
         Current := Current.Next;
      end loop;
      return Count;
   end Length;

   -- Free all nodes
   procedure Clear is
      Current : Node_Ptr;
      Next    : Node_Ptr;
   begin
      Current := Head;
      while Current /= null loop
         Next := Current.Next;
         Free(Current);
         Current := Next;
      end loop;
      Head := null;
   end Clear;

begin
   Put_Line("Linked list example:");

   -- Build list
   Push_Back(10);
   Push_Back(20);
   Push_Back(30);
   Print_List;
   Put_Line("Length:" & Natural'Image(Length));

   Push_Front(5);
   Put_Line("After Push_Front(5):");
   Print_List;

   Push_Back(40);
   Put_Line("After Push_Back(40):");
   Print_List;

   -- Clean up
   Clear;
   Put_Line("After Clear:");
   Print_List;
   Put_Line("Length:" & Natural'Image(Length));
end Linked_List;
