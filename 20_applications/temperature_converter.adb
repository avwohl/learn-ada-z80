-- Example: temperature_converter.adb
-- Concept: Practical utility with strong typing
--
-- Demonstrates: derived types for type safety,
-- functions, formatted output.
-- Uses integer arithmetic (suitable for Z80 without FPU).

with Ada.Text_IO;
use Ada.Text_IO;

procedure Temperature_Converter is

   -- Temperature types (prevents mixing)
   type Celsius is new Integer;
   type Fahrenheit is new Integer;
   type Kelvin is new Integer;

   -- Conversion functions (integer approximations)
   -- F = C * 9/5 + 32
   -- C = (F - 32) * 5/9
   -- K = C + 273

   function To_Fahrenheit(C : Celsius) return Fahrenheit is
   begin
      return Fahrenheit((Integer(C) * 9) / 5 + 32);
   end To_Fahrenheit;

   function To_Celsius(F : Fahrenheit) return Celsius is
   begin
      return Celsius(((Integer(F) - 32) * 5) / 9);
   end To_Celsius;

   function To_Kelvin(C : Celsius) return Kelvin is
   begin
      return Kelvin(Integer(C) + 273);
   end To_Kelvin;

   function From_Kelvin(K : Kelvin) return Celsius is
   begin
      return Celsius(Integer(K) - 273);
   end From_Kelvin;

   procedure Print_Temp(C : Celsius) is
      F : Fahrenheit := To_Fahrenheit(C);
      K : Kelvin := To_Kelvin(C);
   begin
      Put(Celsius'Image(C) & "C = " &
          Fahrenheit'Image(F) & "F = " &
          Kelvin'Image(K) & "K");
      New_Line;
   end Print_Temp;

   C : Celsius;
   F : Fahrenheit;
begin
   Put_Line("=== Temperature Converter ===");
   New_Line;

   -- Show conversions for key temperatures
   Put_Line("Key temperatures:");
   Put("  Absolute zero:  ");
   Print_Temp(Celsius(-273));

   Put("  Water freezes:  ");
   Print_Temp(Celsius(0));

   Put("  Room temp:      ");
   Print_Temp(Celsius(20));

   Put("  Body temp:      ");
   Print_Temp(Celsius(37));

   Put("  Water boils:    ");
   Print_Temp(Celsius(100));

   New_Line;

   -- Conversion table
   Put_Line("Celsius to Fahrenheit table:");
   Put_Line("    C       F");
   Put_Line("  ----   ----");
   C := -20;
   while C <= 40 loop
      F := To_Fahrenheit(C);
      Put("  ");
      Put(Celsius'Image(C));
      Set_Col(10);
      Put(Fahrenheit'Image(F));
      New_Line;
      C := C + 10;
   end loop;

   New_Line;

   -- Type safety demo
   Put_Line("Type safety prevents bugs:");
   Put_Line("  Cannot accidentally mix Celsius and Fahrenheit!");
   -- These would be compile-time errors:
   -- C := F;  -- Error: different types
   -- C := C + F;  -- Error: can't add different types
   Put_Line("  Must use explicit conversion functions.");
end Temperature_Converter;
