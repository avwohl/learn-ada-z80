-- Example: stack_machine.adb
-- Concept: Stack-based calculator (like HP calculators)
--
-- Demonstrates: stacks, records, case statements.
-- RPN (Reverse Polish Notation) calculator.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Stack_Machine is

   Stack_Size : constant := 20;
   type Stack_Array is array(1 .. Stack_Size) of Integer;

   type Calculator is record
      Stack : Stack_Array := (others => 0);
      SP    : Natural := 0;  -- Stack pointer
   end record;

   Stack_Underflow : exception;
   Stack_Overflow  : exception;

   procedure Push(C : in Out Calculator; Value : Integer) is
   begin
      if C.SP >= Stack_Size then
         raise Stack_Overflow;
      end if;
      C.SP := C.SP + 1;
      C.Stack(C.SP) := Value;
   end Push;

   function Pop(C : in Out Calculator) return Integer is
      Value : Integer;
   begin
      if C.SP = 0 then
         raise Stack_Underflow;
      end if;
      Value := C.Stack(C.SP);
      C.SP := C.SP - 1;
      return Value;
   end Pop;

   procedure Execute(C : in Out Calculator; Op : Character) is
      A, B : Integer;
   begin
      case Op is
         when '+' =>
            B := Pop(C);
            A := Pop(C);
            Push(C, A + B);
         when '-' =>
            B := Pop(C);
            A := Pop(C);
            Push(C, A - B);
         when '*' =>
            B := Pop(C);
            A := Pop(C);
            Push(C, A * B);
         when '/' =>
            B := Pop(C);
            A := Pop(C);
            Push(C, A / B);
         when 'd' =>  -- Duplicate top
            A := Pop(C);
            Push(C, A);
            Push(C, A);
         when 's' =>  -- Swap top two
            B := Pop(C);
            A := Pop(C);
            Push(C, B);
            Push(C, A);
         when 'p' =>  -- Pop and print
            A := Pop(C);
            Put_Line("  = " & Integer'Image(A));
            Push(C, A);
         when others =>
            Put_Line("Unknown operation: " & Op);
      end case;
   end Execute;

   procedure Show_Stack(C : Calculator) is
   begin
      Put("Stack [" & Natural'Image(C.SP) & "]: ");
      for I in 1 .. C.SP loop
         Put(Integer'Image(C.Stack(I)) & " ");
      end loop;
      New_Line;
   end Show_Stack;

   Calc : Calculator;
begin
   Put_Line("=== RPN Stack Calculator ===");
   Put_Line("Operations: + - * / d(dup) s(swap) p(print)");
   New_Line;

   -- Example: Calculate (3 + 4) * 5
   Put_Line("Calculating (3 + 4) * 5:");
   Push(Calc, 3);
   Show_Stack(Calc);
   Push(Calc, 4);
   Show_Stack(Calc);
   Execute(Calc, '+');
   Show_Stack(Calc);
   Push(Calc, 5);
   Show_Stack(Calc);
   Execute(Calc, '*');
   Show_Stack(Calc);
   Execute(Calc, 'p');
   New_Line;

   -- Example: Calculate 10 - 3 * 2 (should be 4, not 14)
   Calc.SP := 0;  -- Clear
   Put_Line("Calculating 10 - 3 * 2 (= 10 - 6 = 4):");
   Push(Calc, 10);
   Push(Calc, 3);
   Push(Calc, 2);
   Execute(Calc, '*');
   Execute(Calc, '-');
   Execute(Calc, 'p');
end Stack_Machine;
