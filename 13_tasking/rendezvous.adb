-- Example: rendezvous.adb
-- Concept: Task communication via rendezvous
--
-- Tasks communicate through entries (like procedure calls).
-- Caller waits at entry call, callee waits at accept.
-- When both ready, they "rendezvous" and exchange data.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Rendezvous is

   -- Task with entries
   task Server is
      entry Start;
      entry Process(X : in Integer; Result : out Integer);
      entry Stop;
   end Server;

   task body Server is
      Running : Boolean := False;
   begin
      Put_Line("Server: Waiting for Start");

      accept Start do
         Put_Line("Server: Received Start");
         Running := True;
      end Start;

      while Running loop
         select
            accept Process(X : in Integer; Result : out Integer) do
               Put_Line("Server: Processing" & Integer'Image(X));
               Result := X * 2;
            end Process;
         or
            accept Stop do
               Put_Line("Server: Received Stop");
               Running := False;
            end Stop;
         end select;
      end loop;

      Put_Line("Server: Shutting down");
   end Server;

   R : Integer;
begin
   Put_Line("Main: Starting server");
   Server.Start;  -- Rendezvous with Start entry

   Put_Line("Main: Sending requests");
   Server.Process(10, R);
   Put_Line("Main: Got result" & Integer'Image(R));

   Server.Process(25, R);
   Put_Line("Main: Got result" & Integer'Image(R));

   Server.Process(7, R);
   Put_Line("Main: Got result" & Integer'Image(R));

   Put_Line("Main: Stopping server");
   Server.Stop;  -- Rendezvous with Stop entry

   Put_Line("Main: Done");
end Rendezvous;
