with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_14 is

   subtype Ordinates is Positive;

   type Rock_Types is (Cube, Round);

   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.X < Right.X or (Left.X = Right.x and Left.Y < Right.Y));

   type Rocks is record
      Rock_Type : Rock_Types;
      Coordinate : Coordinates;
   end record; -- Rocks

   package Rock_Stores is new
     Ada.Containers.Ordered_Maps (Coordinates, Rocks);
   -- the key is the origional coordinates and the element contains the new
   -- coordinates.
   use Rock_Stores;

   procedure Read_input (Rock_Store : out Rock_Stores.Map;
                         X_Low, X_High, Y_Low, Y_High : out Ordinates) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Rock : Rocks;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_14.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Rock_Store);
      X_Low := Ordinates'Last;
      X_High := Ordinates'First;
      Y_Low := Ordinates'Last;
      Y_High := Ordinates'First;
      while not End_Of_File (Input_File) loop
         Rock.Coordinate.Y := Ordinates (Line (Input_File));
         Get_Line (Input_File, Text);
         for X in Positive range 1 .. Length (Text) loop
            Rock.Coordinate.X := X;
            if X_Low > X then
               X_Low := X;
            end if; -- X_Low > X
            if X_High < X then
               X_High := X;
            end if; -- X_High < X
            if Y_Low > Rock.Coordinate.Y then
               Y_Low := Rock.Coordinate.Y;
            end if; -- Y_Low > Rock.Coordinate.Y
            if Y_High < Rock.Coordinate.Y then
               Y_High := Rock.Coordinate.Y;
            end if; -- Y_High < Rock.Coordinate.Y
            if Element (Text, X) = '#' then
               Rock.Rock_Type := Cube;
               include (Rock_Store, Rock.Coordinate, Rock);
            elsif  Element (Text, X) = 'O' then
               Rock.Rock_Type := Round;
               include (Rock_Store, Rock.Coordinate, Rock);
            end if; -- Element (Text, X) = '#'
         end loop; -- X in Positive range 1 .. Length (Text)
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   procedure Tilt_North (Rock_Store : in out Rock_Stores.Map;
                         X_Low, X_High, Y_Low, Y_High : in Ordinates) is

      function Next_Free (Rock_Store : in out Rock_Stores.Map;
                          X, Y_Start, Y_Low : in Ordinates) return Ordinates is

         Result : Ordinates := X_Low;

      begin -- Next_Free
         for Y in Ordinates range Y_Low .. Y_Start - 1 loop
            if Contains (Rock_Store, ((X, Y))) then
               if Rock_Store ((X, Y)).Coordinate.Y < Y_Start then
                  Result := Rock_Store ((X, Y)).Coordinate.Y + 1;
               end if; --Rock_Store ((X, Y)).Coordinate.Y
            end if; -- Contains (Rock_Store, ((X, Y)))
         end loop; -- Y in Ordinates range Y_Low .. Y_Start - 1
         return Result;
      end Next_Free;

   begin -- Tilt_North
      for X in Ordinates range X_Low .. X_High loop
         for Y in Ordinates range Y_Low .. Y_High loop
            if  Contains (Rock_Store, (X, Y)) then
               if Rock_Store ((X, Y)).Rock_type = Round then
                  Rock_Store ((X, Y)).Coordinate.Y :=
                    Next_Free (Rock_Store, X, Y, Y_Low);
               end if; -- Rock_Store ((X, Y)).Rock_type = Round
            end if; -- Contains (Rock_Store, (X, Y))
         end loop; -- Y in Ordinates range Y_Low .. Y_High
      end loop; -- X in Ordinates range X_Low .. X_High
   end Tilt_North;

   function Load (Rock_Store : in Rock_Stores.Map;
                  Y_High : in Ordinates) return Positive is

      Result : Natural := 0;

   begin -- Load
      for R in Iterate (Rock_Store) loop
         if Element (R).Rock_Type = Round then
            Result := @ + Y_High - Element (R).Coordinate.Y + 1;
         end if; -- Element (R).Rock_Type = Round
      end loop; -- R in Iterate (Rock_Store)
      return Result;
   end Load;

   Rock_Store : Rock_Stores.Map;
   X_Low, X_High, Y_Low, Y_High : Ordinates;

begin -- December_14
   Read_input (Rock_Store, X_Low, X_High, Y_Low, Y_High);
   Tilt_North (Rock_Store, X_Low, X_High, Y_Low, Y_High);
   Put_Line ("Part one:" & Load (Rock_Store, Y_High)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_14;
