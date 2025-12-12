-- Example: variant_records.adb
-- Concept: Variant records (discriminated records)
--
-- Variant records can have different fields depending
-- on a discriminant value. Like tagged unions in C.
-- The discriminant determines which variant is active.

with Ada.Text_IO;
use Ada.Text_IO;

procedure Variant_Records is
   -- Discriminant controls the variant
   type Shape_Kind is (Circle, Rectangle, Triangle);

   type Shape(Kind : Shape_Kind) is record
      X, Y : Integer;  -- Common fields for all variants
      case Kind is
         when Circle =>
            Radius : Integer;
         when Rectangle =>
            Width, Height : Integer;
         when Triangle =>
            Base, Altitude : Integer;
      end case;
   end record;

   -- Record with discriminant affecting size
   type Buffer(Size : Positive) is record
      Data  : String(1 .. Size);
      Count : Natural := 0;
   end record;

   C : Shape(Circle);
   R : Shape(Rectangle);
   T : Shape(Triangle);

   B : Buffer(20);
begin
   Put_Line("Variant record examples:");

   -- Initialize each variant
   C := (Kind => Circle, X => 10, Y => 10, Radius => 5);
   R := (Kind => Rectangle, X => 0, Y => 0, Width => 100, Height => 50);
   T := (Kind => Triangle, X => 20, Y => 20, Base => 30, Altitude => 15);

   -- Print circle
   Put_Line("Circle:");
   Put_Line("  Position: (" & Integer'Image(C.X) &
            "," & Integer'Image(C.Y) & ")");
   Put_Line("  Radius:" & Integer'Image(C.Radius));

   -- Print rectangle
   Put_Line("Rectangle:");
   Put_Line("  Position: (" & Integer'Image(R.X) &
            "," & Integer'Image(R.Y) & ")");
   Put_Line("  Width:" & Integer'Image(R.Width) &
            " Height:" & Integer'Image(R.Height));

   -- Print triangle
   Put_Line("Triangle:");
   Put_Line("  Position: (" & Integer'Image(T.X) &
            "," & Integer'Image(T.Y) & ")");
   Put_Line("  Base:" & Integer'Image(T.Base) &
            " Altitude:" & Integer'Image(T.Altitude));

   -- Cannot access wrong variant field
   -- C.Width := 10;  -- Error! Circle doesn't have Width

   -- Check variant kind
   Put_Line("Checking variants:");
   Put_Line("  C.Kind = " & Shape_Kind'Image(C.Kind));
   Put_Line("  R.Kind = " & Shape_Kind'Image(R.Kind));

   -- Buffer with discriminant
   B.Data := "Hello Ada World!    ";
   B.Count := 16;
   Put_Line("Buffer (Size=20):");
   Put_Line("  Data: " & B.Data(1 .. B.Count));
   Put_Line("  Count:" & Natural'Image(B.Count));
end Variant_Records;
