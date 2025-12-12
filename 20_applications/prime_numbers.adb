-- Example: prime_numbers.adb
-- Concept: Prime number algorithms
--
-- Demonstrates: loops, functions, arrays, algorithms.
-- Classic computer science problem suitable for Z80.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Prime_Numbers is

   -- Simple primality test
   function Is_Prime(N : Positive) return Boolean is
   begin
      if N < 2 then
         return False;
      elsif N = 2 then
         return True;
      elsif N mod 2 = 0 then
         return False;
      else
         -- Check odd divisors up to sqrt(N)
         for I in 3 .. N / 2 loop
            if I * I > N then
               exit;
            end if;
            if N mod I = 0 then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end Is_Prime;

   -- Sieve of Eratosthenes (find all primes up to limit)
   procedure Print_Primes_Sieve(Limit : Positive) is
      Is_Composite : array(2 .. Limit) of Boolean := (others => False);
      Count : Natural := 0;
   begin
      -- Mark composites
      for I in 2 .. Limit loop
         if not Is_Composite(I) then
            -- I is prime; mark its multiples as composite
            declare
               Multiple : Integer := I * 2;
            begin
               while Multiple <= Limit loop
                  Is_Composite(Multiple) := True;
                  Multiple := Multiple + I;
               end loop;
            end;
         end if;
      end loop;

      -- Print primes
      Put("Primes up to" & Positive'Image(Limit) & ": ");
      for I in 2 .. Limit loop
         if not Is_Composite(I) then
            Put(Positive'Image(I));
            Count := Count + 1;
         end if;
      end loop;
      New_Line;
      Put_Line("Found" & Natural'Image(Count) & " primes");
   end Print_Primes_Sieve;

   -- Find nth prime
   function Nth_Prime(N : Positive) return Positive is
      Count : Natural := 0;
      Candidate : Positive := 1;
   begin
      while Count < N loop
         Candidate := Candidate + 1;
         if Is_Prime(Candidate) then
            Count := Count + 1;
         end if;
      end loop;
      return Candidate;
   end Nth_Prime;

begin
   Put_Line("=== Prime Number Examples ===");
   New_Line;

   -- Test Is_Prime
   Put_Line("Testing Is_Prime function:");
   for N in 1 .. 20 loop
      if Is_Prime(N) then
         Put(Positive'Image(N));
      end if;
   end loop;
   New_Line;
   New_Line;

   -- Sieve of Eratosthenes
   Put_Line("Sieve of Eratosthenes:");
   Print_Primes_Sieve(50);
   New_Line;

   -- Nth prime
   Put_Line("Finding nth prime:");
   for N in 1 .. 10 loop
      Put_Line("  Prime #" & Positive'Image(N) &
               " is" & Positive'Image(Nth_Prime(N)));
   end loop;
   New_Line;

   -- Check specific numbers
   Put_Line("Primality tests:");
   Put_Line("  97 is prime: " & Boolean'Image(Is_Prime(97)));
   Put_Line("  100 is prime: " & Boolean'Image(Is_Prime(100)));
   Put_Line("  101 is prime: " & Boolean'Image(Is_Prime(101)));
end Prime_Numbers;
