-- Example: reserved_words.adb
-- Concept: Ada's reserved words (keywords)
--
-- Ada has many reserved words that cannot be used as identifiers.
-- This program demonstrates many of them in context.
-- Reserved words are always lowercase in Ada source code.
--
-- Common reserved words include:
--   abort, abs, abstract, accept, access, aliased, all, and,
--   array, at, begin, body, case, constant, declare, delay,
--   delta, digits, do, else, elsif, end, entry, exception,
--   exit, for, function, generic, goto, if, in, interface,
--   is, limited, loop, mod, new, not, null, of, or, others,
--   out, overriding, package, pragma, private, procedure,
--   protected, raise, range, record, rem, renames, requeue,
--   return, reverse, select, separate, some, subtype, synchronized,
--   tagged, task, terminate, then, type, until, use, when,
--   while, with, xor

with Ada.Text_IO;
use Ada.Text_IO;

procedure Reserved_Words is
   -- "constant" is a reserved word
   Max_Value : constant Integer := 100;

   -- "type" is reserved - used to declare new types
   type Small_Int is range 0 .. 255;

   Value : Small_Int := 50;
begin
   -- "if", "then", "else", "end" are reserved
   if Value > 25 then
      Put_Line("Value is greater than 25");
   else
      Put_Line("Value is 25 or less");
   end if;

   -- "for", "in", "loop" are reserved
   for I in 1 .. 3 loop
      Put_Line("Loop iteration" & Integer'Image(I));
   end loop;

   Put_Line("Ada has many reserved words!");
end Reserved_Words;
