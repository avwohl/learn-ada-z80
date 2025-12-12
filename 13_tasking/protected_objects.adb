-- Example: protected_objects.adb
-- Concept: Protected objects for shared data
--
-- Protected objects provide thread-safe access to data.
-- Functions: concurrent read access
-- Procedures: exclusive write access
-- Entries: conditional access (with barriers)

with Ada.Text_IO;
use Ada.Text_IO;

procedure Protected_Objects is

   -- Protected counter
   protected Counter is
      procedure Increment;
      procedure Decrement;
      function Value return Integer;
      procedure Reset;
   private
      Count : Integer := 0;
   end Counter;

   protected body Counter is
      procedure Increment is
      begin
         Count := Count + 1;
      end Increment;

      procedure Decrement is
      begin
         Count := Count - 1;
      end Decrement;

      function Value return Integer is
      begin
         return Count;
      end Value;

      procedure Reset is
      begin
         Count := 0;
      end Reset;
   end Counter;

   -- Protected bounded buffer with entry
   protected Buffer is
      entry Put(Item : in Integer);
      entry Get(Item : out Integer);
      function Count return Natural;
   private
      Data   : array(1 .. 5) of Integer;
      Head   : Positive := 1;
      Tail   : Positive := 1;
      Size   : Natural := 0;
   end Buffer;

   protected body Buffer is
      entry Put(Item : in Integer) when Size < 5 is
      begin
         Data(Tail) := Item;
         Tail := (Tail mod 5) + 1;
         Size := Size + 1;
      end Put;

      entry Get(Item : out Integer) when Size > 0 is
      begin
         Item := Data(Head);
         Head := (Head mod 5) + 1;
         Size := Size - 1;
      end Get;

      function Count return Natural is
      begin
         return Size;
      end Count;
   end Buffer;

   -- Tasks using protected objects
   task Incrementer;
   task Decrementer;

   task body Incrementer is
   begin
      for I in 1 .. 5 loop
         Counter.Increment;
         Put_Line("Inc: Counter =" & Integer'Image(Counter.Value));
      end loop;
   end Incrementer;

   task body Decrementer is
   begin
      for I in 1 .. 3 loop
         Counter.Decrement;
         Put_Line("Dec: Counter =" & Integer'Image(Counter.Value));
      end loop;
   end Decrementer;

begin
   Put_Line("Protected objects example:");

   -- Counter example (tasks will run)
   Put_Line("Counter operations:");

   -- Buffer example (after tasks complete)
   Put_Line("Buffer operations:");
   Buffer.Put(10);
   Buffer.Put(20);
   Buffer.Put(30);
   Put_Line("  Buffer count:" & Natural'Image(Buffer.Count));

   declare
      V : Integer;
   begin
      Buffer.Get(V);
      Put_Line("  Got:" & Integer'Image(V));
      Buffer.Get(V);
      Put_Line("  Got:" & Integer'Image(V));
   end;

   Put_Line("  Final buffer count:" & Natural'Image(Buffer.Count));
end Protected_Objects;
