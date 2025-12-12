-- Example: exception_info.adb
-- Concept: Getting exception information
--
-- Ada provides ways to get information about exceptions:
-- - Exception_Name: the name of the exception
-- - Exception_Message: message attached when raised
-- - Exception_Information: full details (implementation-defined)

with Ada.Text_IO;
with Ada.Exceptions;
use Ada.Text_IO;
use Ada.Exceptions;

procedure Exception_Info is

   My_Error : exception;

   procedure Cause_Error(Which : Integer) is
   begin
      case Which is
         when 1 =>
            raise My_Error with "Custom error message";
         when 2 =>
            raise Constraint_Error with "Value out of range";
         when others =>
            declare
               X : Integer := 1;
               Y : Integer := 0;
            begin
               X := X / Y;  -- No message
            end;
      end case;
   end Cause_Error;

begin
   Put_Line("Exception information examples:");

   -- Catch and examine exception
   for I in 1 .. 3 loop
      Put_Line("Test" & Integer'Image(I) & ":");
      begin
         Cause_Error(I);
      exception
         when E : others =>
            Put_Line("  Exception_Name: " & Exception_Name(E));
            Put_Line("  Exception_Message: " & Exception_Message(E));
            -- Exception_Information may include stack trace on some systems
            Put_Line("  Exception_Information:");
            Put_Line("    " & Exception_Information(E));
      end;
      New_Line;
   end loop;

   -- Raise with message
   Put_Line("Raise with message:");
   begin
      raise Program_Error with "Something went wrong!";
   exception
      when E : Program_Error =>
         Put_Line("  Caught: " & Exception_Message(E));
   end;

   -- Using exception occurrence
   Put_Line("Saving exception occurrence:");
   declare
      Saved : Exception_Occurrence;
   begin
      begin
         raise My_Error with "Saved for later";
      exception
         when E : others =>
            Save_Occurrence(Saved, E);
            Put_Line("  Exception saved");
      end;

      -- Can examine saved exception later
      Put_Line("  Saved exception: " & Exception_Name(Saved));
      Put_Line("  Saved message: " & Exception_Message(Saved));
   end;
end Exception_Info;
