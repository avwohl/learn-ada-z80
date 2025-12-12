-- Example: stack_pkg.ads
-- Concept: Package specification (interface)
--
-- The specification (.ads file) declares what the package provides.
-- It defines the public interface that clients can use.
-- Implementation details are hidden in the body.

package Stack_Pkg is

   -- Public constants
   Max_Size : constant := 100;

   -- Public type (visible but implementation hidden)
   type Stack is private;

   -- Public exceptions
   Stack_Empty : exception;
   Stack_Full  : exception;

   -- Public operations (the interface)
   procedure Push(S : in out Stack; Item : Integer);
   procedure Pop(S : in Out Stack; Item : out Integer);
   function Top(S : Stack) return Integer;
   function Is_Empty(S : Stack) return Boolean;
   function Is_Full(S : Stack) return Boolean;
   function Size(S : Stack) return Natural;
   procedure Clear(S : in Out Stack);

private
   -- Private part: implementation details hidden from clients

   type Item_Array is array(1 .. Max_Size) of Integer;

   type Stack is record
      Items : Item_Array;
      Count : Natural := 0;  -- Number of items currently on stack
   end record;

   -- Note: Clients know Stack is a record but can't access its fields
   -- They must use the public operations above

end Stack_Pkg;
