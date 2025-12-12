-- Example: access_parameters.adb
-- Concept: Access parameters and aliased objects
--
-- Access parameters allow passing pointers to subprograms.
-- "aliased" allows taking 'Access of local variables.
-- "access" in parameters passes by reference.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Access_Parameters is

   type Int_Ptr is access all Integer;  -- "all" for general access

   -- Procedure that takes an access parameter
   procedure Double_It(P : access Integer) is
   begin
      P.all := P.all * 2;
   end Double_It;

   -- Function that returns access to max of two
   function Max_Ptr(A, B : access Integer) return access Integer is
   begin
      if A.all > B.all then
         return A;
      else
         return B;
      end if;
   end Max_Ptr;

   -- Aliased variables can have 'Access taken
   X : aliased Integer := 10;
   Y : aliased Integer := 20;

   P : Int_Ptr;
   Result : access Integer;
begin
   Put_Line("Access parameters examples:");

   -- Get access to aliased variable
   P := X'Access;
   Put_Line("X'Access:");
   Put_Line("  X =" & Integer'Image(X));
   Put_Line("  P.all =" & Integer'Image(P.all));

   -- Modify through access
   P.all := 100;
   Put_Line("After P.all := 100:");
   Put_Line("  X =" & Integer'Image(X));

   -- Pass aliased variable to access parameter
   X := 25;
   Put_Line("Before Double_It: X =" & Integer'Image(X));
   Double_It(X'Access);
   Put_Line("After Double_It: X =" & Integer'Image(X));

   -- Return access from function
   X := 30;
   Y := 50;
   Result := Max_Ptr(X'Access, Y'Access);
   Put_Line("Max of X=" & Integer'Image(X) &
            " and Y=" & Integer'Image(Y) &
            " is" & Integer'Image(Result.all));

   -- Modify through returned access
   Result.all := 999;
   Put_Line("After setting max to 999:");
   Put_Line("  X =" & Integer'Image(X));
   Put_Line("  Y =" & Integer'Image(Y) & " (was the max)");

   -- Anonymous access type in declaration
   declare
      procedure Increment(P : not null access Integer) is
      begin
         P.all := P.all + 1;
      end Increment;

      Z : aliased Integer := 0;
   begin
      Put_Line("Anonymous access with 'not null':");
      for I in 1 .. 5 loop
         Increment(Z'Access);
         Put("  Z =" & Integer'Image(Z));
      end loop;
      New_Line;
   end;
end Access_Parameters;
