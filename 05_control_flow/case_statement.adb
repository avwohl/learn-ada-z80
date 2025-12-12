-- Example: case_statement.adb
-- Concept: Case (switch) statements
--
-- Ada case statement must cover ALL possible values.
-- Use "when others =>" for default/remaining cases.
-- Works with discrete types: integers, characters, enumerations.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Case_Statement is
   type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

   Today : Day := Wed;
   Grade : Character := 'B';
   Month : Integer := 7;
begin
   Put_Line("Case statement examples:");

   -- Case with enumeration
   Put("  Today is ");
   case Today is
      when Mon =>
         Put_Line("Monday - Start of week");
      when Tue =>
         Put_Line("Tuesday");
      when Wed =>
         Put_Line("Wednesday - Midweek");
      when Thu =>
         Put_Line("Thursday");
      when Fri =>
         Put_Line("Friday - Almost weekend!");
      when Sat | Sun =>   -- Multiple choices with |
         Put_Line("Weekend!");
   end case;

   -- Case with character
   Put("  Grade " & Grade & " means: ");
   case Grade is
      when 'A' =>
         Put_Line("Excellent");
      when 'B' =>
         Put_Line("Good");
      when 'C' =>
         Put_Line("Average");
      when 'D' =>
         Put_Line("Below average");
      when 'F' =>
         Put_Line("Failing");
      when others =>   -- Covers all other characters
         Put_Line("Invalid grade");
   end case;

   -- Case with integer ranges
   Put("  Month" & Integer'Image(Month) & " is in ");
   case Month is
      when 1 | 2 | 12 =>
         Put_Line("Winter");
      when 3 .. 5 =>      -- Range with ..
         Put_Line("Spring");
      when 6 .. 8 =>
         Put_Line("Summer");
      when 9 .. 11 =>
         Put_Line("Fall");
      when others =>
         Put_Line("Invalid month");
   end case;

   -- All cases must be covered - this is enforced by compiler
end Case_Statement;
