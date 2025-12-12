-- Example: pragma_examples.adb
-- Concept: Pragmas (compiler directives)
--
-- Pragmas provide instructions to the compiler.
-- They don't change program semantics but affect compilation.
-- Common pragmas: Assert, Inline, Suppress, Volatile, etc.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Pragma_Examples is

   pragma Suppress(All_Checks);  -- DEMO ONLY - not recommended!
   -- This suppresses runtime checks for this procedure

   -- Inline pragma suggests inlining a subprogram
   function Square(N : Integer) return Integer is
   begin
      return N * N;
   end Square;
   pragma Inline(Square);

   -- Assert pragma for runtime checks
   X : Integer := 10;

   -- Unreferenced pragma silences warnings
   Unused : Integer := 42;
   pragma Unreferenced(Unused);

   -- Volatile for hardware registers
   -- pragma Volatile(Some_Hardware_Register);

begin
   Put_Line("Pragma examples:");

   -- Assert checks condition at runtime
   Put_Line("Assert examples:");
   pragma Assert(X > 0, "X must be positive");
   Put_Line("  Assert(X > 0) passed");

   pragma Assert(X = 10);
   Put_Line("  Assert(X = 10) passed");

   -- This would fail if X were not 10:
   -- pragma Assert(X = 5, "X should be 5");

   -- Debug pragma (implementation-defined)
   pragma Debug(Put_Line("  Debug: X =" & Integer'Image(X)));

   -- Compile time warning
   -- pragma Compile_Time_Warning(True, "This is a warning");

   -- Using inlined function
   Put_Line("Using inlined Square function:");
   Put_Line("  Square(5) =" & Integer'Image(Square(5)));

   -- Common pragmas:
   Put_Line("Common pragmas:");
   Put_Line("  pragma Assert     - Runtime assertion");
   Put_Line("  pragma Inline     - Suggest inlining");
   Put_Line("  pragma Suppress   - Disable specific checks");
   Put_Line("  pragma Volatile   - Prevent optimization of variable");
   Put_Line("  pragma Atomic     - Indivisible read/write");
   Put_Line("  pragma Pack       - Minimize storage");
   Put_Line("  pragma Pure       - No side effects");
   Put_Line("  pragma Preelaborate - Can be elaborated early");
   Put_Line("  pragma Import     - Import from another language");
   Put_Line("  pragma Export     - Export to another language");
end Pragma_Examples;
