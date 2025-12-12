-- Example: case_expression.adb
-- Concept: Case expressions (Ada 2012)
--
-- Case expressions return values based on selector.
-- Like switch expressions in other languages.
-- All alternatives must return same type.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Case_Expression is

   type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

   function Day_Type(D : Day) return String is
     (case D is
        when Mon .. Fri => "Weekday",
        when Sat | Sun => "Weekend");

   function Month_Days(M : Integer) return Integer is
     (case M is
        when 1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        when 4 | 6 | 9 | 11 => 30,
        when 2 => 28,
        when others => 0);

   function Grade_Points(G : Character) return Integer is
     (case G is
        when 'A' => 4,
        when 'B' => 3,
        when 'C' => 2,
        when 'D' => 1,
        when 'F' => 0,
        when others => -1);

   Today : Day := Wed;
   Points : Integer;
begin
   Put_Line("Case expression examples (Ada 2012):");

   -- Day type
   Put_Line("Day types:");
   for D in Day loop
      Put_Line("  " & Day'Image(D) & " is a " & Day_Type(D));
   end loop;
   New_Line;

   -- Month days
   Put_Line("Days in each month:");
   for M in 1 .. 12 loop
      Put_Line("  Month" & Integer'Image(M) & ":" &
               Integer'Image(Month_Days(M)) & " days");
   end loop;
   New_Line;

   -- Grade points
   Put_Line("Grade points:");
   for G in Character range 'A' .. 'F' loop
      Points := Grade_Points(G);
      if Points >= 0 then
         Put_Line("  " & G & " =" & Integer'Image(Points) & " points");
      end if;
   end loop;
   New_Line;

   -- Inline case expression
   Put_Line("Traffic light actions:");
   for Light in Integer range 1 .. 3 loop
      Put_Line("  Light" & Integer'Image(Light) & ": " &
               (case Light is
                  when 1 => "Stop (Red)",
                  when 2 => "Caution (Yellow)",
                  when 3 => "Go (Green)",
                  when others => "Error"));
   end loop;
end Case_Expression;
