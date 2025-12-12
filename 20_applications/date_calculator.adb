-- Example: date_calculator.adb
-- Concept: Date handling with records and functions
--
-- Demonstrates: records, functions, validation,
-- modular arithmetic. Useful utilities.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Date_Calculator is

   subtype Year_Number is Integer range 1 .. 9999;
   subtype Month_Number is Integer range 1 .. 12;
   subtype Day_Number is Integer range 1 .. 31;

   type Date is record
      Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number;
   end record;

   function Is_Leap_Year(Y : Year_Number) return Boolean is
   begin
      return (Y mod 4 = 0 and Y mod 100 /= 0) or (Y mod 400 = 0);
   end Is_Leap_Year;

   function Days_In_Month(Y : Year_Number; M : Month_Number) return Day_Number is
   begin
      case M is
         when 1 | 3 | 5 | 7 | 8 | 10 | 12 => return 31;
         when 4 | 6 | 9 | 11 => return 30;
         when 2 =>
            if Is_Leap_Year(Y) then
               return 29;
            else
               return 28;
            end if;
      end case;
   end Days_In_Month;

   function Is_Valid(D : Date) return Boolean is
   begin
      return D.Day <= Days_In_Month(D.Year, D.Month);
   end Is_Valid;

   function Day_Of_Year(D : Date) return Positive is
      Result : Positive := D.Day;
   begin
      for M in 1 .. D.Month - 1 loop
         Result := Result + Days_In_Month(D.Year, M);
      end loop;
      return Result;
   end Day_Of_Year;

   function Days_In_Year(Y : Year_Number) return Positive is
   begin
      if Is_Leap_Year(Y) then
         return 366;
      else
         return 365;
      end if;
   end Days_In_Year;

   -- Zeller's congruence for day of week (0=Sat, 1=Sun, ..., 6=Fri)
   function Day_Of_Week(D : Date) return Natural is
      Y : Integer := D.Year;
      M : Integer := D.Month;
      Q : constant Integer := D.Day;
      K, J, H : Integer;
   begin
      if M < 3 then
         M := M + 12;
         Y := Y - 1;
      end if;
      K := Y mod 100;
      J := Y / 100;
      H := (Q + (13 * (M + 1)) / 5 + K + K / 4 + J / 4 - 2 * J) mod 7;
      if H < 0 then
         H := H + 7;
      end if;
      return H;
   end Day_Of_Week;

   function Day_Name(D : Date) return String is
      DOW : constant Natural := Day_Of_Week(D);
   begin
      case DOW is
         when 0 => return "Saturday";
         when 1 => return "Sunday";
         when 2 => return "Monday";
         when 3 => return "Tuesday";
         when 4 => return "Wednesday";
         when 5 => return "Thursday";
         when 6 => return "Friday";
         when others => return "Unknown";
      end case;
   end Day_Name;

   procedure Print_Date(D : Date) is
   begin
      Put(Integer'Image(D.Year) & "-" &
          Integer'Image(D.Month) & "-" &
          Integer'Image(D.Day));
   end Print_Date;

   D1, D2 : Date;
begin
   Put_Line("=== Date Calculator ===");
   New_Line;

   -- Test leap years
   Put_Line("Leap year tests:");
   Put_Line("  2000 is leap: " & Boolean'Image(Is_Leap_Year(2000)));
   Put_Line("  2024 is leap: " & Boolean'Image(Is_Leap_Year(2024)));
   Put_Line("  2100 is leap: " & Boolean'Image(Is_Leap_Year(2100)));
   Put_Line("  2023 is leap: " & Boolean'Image(Is_Leap_Year(2023)));
   New_Line;

   -- Days in months
   Put_Line("Days in Feb 2024: " & Day_Number'Image(Days_In_Month(2024, 2)));
   Put_Line("Days in Feb 2023: " & Day_Number'Image(Days_In_Month(2023, 2)));
   New_Line;

   -- Day of year
   D1 := (2024, 12, 31);
   Put("Day of year for ");
   Print_Date(D1);
   Put_Line(":" & Positive'Image(Day_Of_Year(D1)));

   D1 := (2024, 7, 4);
   Put("Day of year for ");
   Print_Date(D1);
   Put_Line(":" & Positive'Image(Day_Of_Year(D1)));
   New_Line;

   -- Day of week
   Put_Line("Day of week:");
   D1 := (2024, 12, 25);  -- Christmas 2024
   Put("  ");
   Print_Date(D1);
   Put_Line(" is " & Day_Name(D1));

   D1 := (2025, 1, 1);  -- New Year 2025
   Put("  ");
   Print_Date(D1);
   Put_Line(" is " & Day_Name(D1));

   D1 := (1969, 7, 20);  -- Moon landing
   Put("  ");
   Print_Date(D1);
   Put_Line(" is " & Day_Name(D1));
end Date_Calculator;
