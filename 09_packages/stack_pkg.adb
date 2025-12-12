-- Example: stack_pkg.adb
-- Concept: Package body (implementation)
--
-- The body (.adb file) implements the operations declared
-- in the specification. It contains the actual code.

package body Stack_Pkg is

   procedure Push(S : in Out Stack; Item : Integer) is
   begin
      if S.Count >= Max_Size then
         raise Stack_Full;
      end if;
      S.Count := S.Count + 1;
      S.Items(S.Count) := Item;
   end Push;

   procedure Pop(S : in Out Stack; Item : out Integer) is
   begin
      if S.Count = 0 then
         raise Stack_Empty;
      end if;
      Item := S.Items(S.Count);
      S.Count := S.Count - 1;
   end Pop;

   function Top(S : Stack) return Integer is
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

   function Is_Full(S : Stack) return Boolean is
   begin
      return S.Count >= Max_Size;
   end Is_Full;

   function Size(S : Stack) return Natural is
   begin
      return S.Count;
   end Size;

   procedure Clear(S : in Out Stack) is
   begin
      S.Count := 0;
   end Clear;

end Stack_Pkg;
