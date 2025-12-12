-- Example: number_guessing.adb
-- Concept: Simple game demonstrating I/O and logic
--
-- Shows: loops, conditionals, random numbers (simulated),
-- user interaction pattern (automated for demo).

with Ada.Text_IO;
use Ada.Text_IO;

procedure Number_Guessing is

   -- Simple pseudo-random number generator
   -- (Linear congruential generator)
   Seed : Integer := 12345;

   function Random(Max : Positive) return Positive is
   begin
      -- LCG: next = (a * seed + c) mod m
      Seed := ((Seed * 1103515245 + 12345) mod 2147483647);
      if Seed < 0 then
         Seed := -Seed;
      end if;
      return (Seed mod Max) + 1;
   end Random;

   -- Game logic
   procedure Play_Game(Secret : Positive; Max_Guesses : Positive) is
      Guess : Positive;
      Guesses_Left : Natural := Max_Guesses;
      Won : Boolean := False;
   begin
      Put_Line("I'm thinking of a number between 1 and 100.");
      Put_Line("You have" & Natural'Image(Max_Guesses) & " guesses.");
      New_Line;

      -- Simulate player guesses using binary search strategy
      declare
         Low : Positive := 1;
         High : Positive := 100;
      begin
         while Guesses_Left > 0 and not Won loop
            Guess := (Low + High) / 2;  -- Binary search guess
            Guesses_Left := Guesses_Left - 1;

            Put_Line("Guess #" & Natural'Image(Max_Guesses - Guesses_Left) &
                     ": " & Positive'Image(Guess));

            if Guess = Secret then
               Won := True;
               Put_Line("  Correct! You win!");
            elsif Guess < Secret then
               Put_Line("  Too low!");
               Low := Guess + 1;
            else
               Put_Line("  Too high!");
               High := Guess - 1;
            end if;
         end loop;
      end;

      if not Won then
         New_Line;
         Put_Line("Out of guesses! The number was" &
                  Positive'Image(Secret));
      else
         Put_Line("Won in" & Natural'Image(Max_Guesses - Guesses_Left) &
                  " guesses!");
      end if;
   end Play_Game;

   Secret_Number : Positive;
begin
   Put_Line("=== Number Guessing Game ===");
   New_Line;

   -- Play a few rounds
   for Round in 1 .. 3 loop
      Put_Line("--- Round" & Positive'Image(Round) & " ---");
      Secret_Number := Random(100);
      Put_Line("[Debug: Secret is" & Positive'Image(Secret_Number) & "]");
      New_Line;
      Play_Game(Secret_Number, 7);
      New_Line;
   end loop;

   Put_Line("Thanks for playing!");
end Number_Guessing;
