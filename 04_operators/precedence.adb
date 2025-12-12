-- Example: precedence.adb
-- Concept: Operator precedence in Ada
--
-- Ada operator precedence (highest to lowest):
--   **  abs  not              (highest)
--   *  /  mod  rem
--   +  -  &                   (unary and binary)
--   =  /=  <  <=  >  >=  in  not in
--   and  or  xor  and then  or else  (lowest)
--
-- Use parentheses to make precedence explicit!

with Ada.Text_IO;
use Ada.Text_IO;

procedure Precedence is
   R : Integer;
   B : Boolean;
begin
   Put_Line("Operator precedence examples:");

   -- ** has highest precedence
   R := 2 + 3 ** 2;      -- 2 + (3**2) = 2 + 9 = 11
   Put_Line("  2 + 3 ** 2 = " & Integer'Image(R) & " (** before +)");

   -- * / before + -
   R := 2 + 3 * 4;       -- 2 + (3*4) = 2 + 12 = 14
   Put_Line("  2 + 3 * 4 = " & Integer'Image(R) & " (* before +)");

   R := 10 - 6 / 2;      -- 10 - (6/2) = 10 - 3 = 7
   Put_Line("  10 - 6 / 2 = " & Integer'Image(R) & " (/ before -)");

   -- Left to right for same precedence
   R := 20 / 4 / 2;      -- (20/4)/2 = 5/2 = 2
   Put_Line("  20 / 4 / 2 = " & Integer'Image(R) & " (left to right)");

   -- Comparison before logical
   B := 3 < 5 and 7 < 10;  -- (3<5) and (7<10)
   Put_Line("  3 < 5 and 7 < 10 = " & Boolean'Image(B));

   -- Use parentheses for clarity
   R := (2 + 3) * 4;     -- 5 * 4 = 20
   Put_Line("  (2 + 3) * 4 = " & Integer'Image(R) & " (parens change order)");

   -- Complex expression
   R := 2 ** 3 * 4 + 5;  -- ((2**3) * 4) + 5 = 8*4+5 = 37
   Put_Line("  2 ** 3 * 4 + 5 = " & Integer'Image(R));

   -- Unary minus
   R := -3 ** 2;         -- -(3**2) = -9
   Put_Line("  -3 ** 2 = " & Integer'Image(R) & " (** before unary -)");

   R := (-3) ** 2;       -- (-3)**2 = 9
   Put_Line("  (-3) ** 2 = " & Integer'Image(R) & " (parens first)");

   Put_Line("When in doubt, use parentheses!");
end Precedence;
