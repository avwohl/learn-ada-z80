-- Example: user_exceptions.adb
-- Concept: User-defined exceptions
--
-- You can declare your own exceptions for domain-specific errors.
-- Use "raise" to signal the exception.

with Ada.Text_IO;
use Ada.Text_IO;

procedure User_Exceptions is

   -- Declare custom exceptions
   Invalid_Input  : exception;
   Value_Too_High : exception;
   Value_Too_Low  : exception;

   -- Procedure that raises exceptions
   procedure Validate(N : Integer) is
   begin
      if N < 0 then
         raise Value_Too_Low;
      elsif N > 100 then
         raise Value_Too_High;
      end if;
      Put_Line("  Value" & Integer'Image(N) & " is valid");
   end Validate;

   -- Function that validates input
   function Parse_Age(S : String) return Natural is
   begin
      -- Simple check for digit string
      for C of S loop
         if C not in '0' .. '9' then
            raise Invalid_Input;
         end if;
      end loop;

      declare
         Age : Natural := 0;
      begin
         for C of S loop
            Age := Age * 10 + (Character'Pos(C) - Character'Pos('0'));
         end loop;
         return Age;
      end;
   end Parse_Age;

begin
   Put_Line("User-defined exceptions:");

   -- Test valid values
   Put_Line("Testing Validate:");
   begin
      Validate(50);
      Validate(0);
      Validate(100);
   exception
      when Value_Too_Low =>
         Put_Line("  Error: Value too low!");
      when Value_Too_High =>
         Put_Line("  Error: Value too high!");
   end;

   -- Test invalid value
   Put_Line("Testing with invalid value 150:");
   begin
      Validate(150);
   exception
      when Value_Too_High =>
         Put_Line("  Caught Value_Too_High!");
   end;

   Put_Line("Testing with invalid value -5:");
   begin
      Validate(-5);
   exception
      when Value_Too_Low =>
         Put_Line("  Caught Value_Too_Low!");
   end;

   -- Test Parse_Age
   Put_Line("Testing Parse_Age:");
   begin
      Put_Line("  Parse_Age(""25"") =" & Natural'Image(Parse_Age("25")));
      Put_Line("  Parse_Age(""abc"") = " & Natural'Image(Parse_Age("abc")));
   exception
      when Invalid_Input =>
         Put_Line("  Caught Invalid_Input!");
   end;
end User_Exceptions;
