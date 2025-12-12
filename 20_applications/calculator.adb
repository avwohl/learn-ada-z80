-- Example: calculator.adb
-- Concept: Simple calculator application
--
-- Demonstrates combining concepts: types, arrays,
-- case statements, exceptions, and I/O.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Calculator is

   type Operation is (Add, Subtract, Multiply, Divide, Modulo);

   function Calculate(A : Integer;
                      Op : Operation;
                      B : Integer) return Integer is
   begin
      case Op is
         when Add =>
            return A + B;
         when Subtract =>
            return A - B;
         when Multiply =>
            return A * B;
         when Divide =>
            if B = 0 then
               raise Constraint_Error with "Division by zero";
            end if;
            return A / B;
         when Modulo =>
            if B = 0 then
               raise Constraint_Error with "Modulo by zero";
            end if;
            return A mod B;
      end case;
   end Calculate;

   function Op_Symbol(Op : Operation) return Character is
   begin
      case Op is
         when Add      => return '+';
         when Subtract => return '-';
         when Multiply => return '*';
         when Divide   => return '/';
         when Modulo   => return '%';
      end case;
   end Op_Symbol;

   A, B, Result : Integer;
begin
   Put_Line("=== Ada Calculator ===");
   New_Line;

   -- Demonstrate calculations
   A := 42;
   B := 7;

   Put_Line("Operations with A =" & Integer'Image(A) &
            " and B =" & Integer'Image(B) & ":");
   New_Line;

   for Op in Operation loop
      begin
         Result := Calculate(A, Op, B);
         Put_Line("  " & Integer'Image(A) & " " &
                  Op_Symbol(Op) & Integer'Image(B) &
                  " =" & Integer'Image(Result));
      exception
         when E : Constraint_Error =>
            Put_Line("  " & Integer'Image(A) & " " &
                     Op_Symbol(Op) & Integer'Image(B) &
                     " = Error!");
      end;
   end loop;

   New_Line;
   Put_Line("Division by zero handling:");
   begin
      Result := Calculate(10, Divide, 0);
      Put_Line("  Result:" & Integer'Image(Result));
   exception
      when Constraint_Error =>
         Put_Line("  Caught division by zero error!");
   end;

   New_Line;
   Put_Line("More examples:");
   Put_Line("  100 + 50 =" & Integer'Image(Calculate(100, Add, 50)));
   Put_Line("  100 - 50 =" & Integer'Image(Calculate(100, Subtract, 50)));
   Put_Line("  12 * 12 =" & Integer'Image(Calculate(12, Multiply, 12)));
   Put_Line("  100 / 7 =" & Integer'Image(Calculate(100, Divide, 7)));
   Put_Line("  100 % 7 =" & Integer'Image(Calculate(100, Modulo, 7)));
end Calculator;
