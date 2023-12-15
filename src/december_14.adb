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

   package Rock_Stores is new
     Ada.Containers.Ordered_Maps (Coordinates, Rock_Types);
   -- the key is the origional coordinates and the element contains the new
   -- coordinates.
   use Rock_Stores;

   type Directions is (North, West, South, East);

   procedure Read_input (Rock_Store : out Rock_Stores.Map;
                         X_High, Y_High : out Ordinates) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Y : Ordinates;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_14.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Rock_Store);
      X_High := Ordinates'First;
      Y_High := Ordinates'First;
      while not End_Of_File (Input_File) loop
         Y := Ordinates (Line (Input_File));
         Get_Line (Input_File, Text);
         for X in Positive range 1 .. Length (Text) loop
            if X_High < X then
               X_High := X;
            end if; -- X_High < X
            if Y_High < Y then
               Y_High := Y;
            end if; -- Y_High < Y
            if Element (Text, X) = '#' then
               insert (Rock_Store, (X, Y), Cube);
            elsif  Element (Text, X) = 'O' then
               insert (Rock_Store, (X, Y), Round);
            end if; -- Element (Text, X) = '#'
         end loop; -- X in Positive range 1 .. Length (Text)
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   procedure Tilt (Rock_Store : in out Rock_Stores.Map;
                   Direction : in Directions;
                   X_High, Y_High : in Ordinates) is

      First_Free : Ordinates;
   begin -- Tilt
      case Direction is
         when North =>
            for X in Ordinates range Ordinates'First .. X_High loop
               First_Free := Ordinates'First;
               for Y in Ordinates range Ordinates'First .. Y_High loop
                  if  Contains (Rock_Store, (X, Y)) then
                     if Rock_Store ((X, Y)) = Round then
                        if Y > First_Free then
                           insert (Rock_Store, (X, First_Free), Round);
                           First_Free := @ + 1;
                           Delete (Rock_Store, (X, Y));
                        else
                           First_Free := Y + 1;
                        end if; -- Y > First_Free
                     else
                        First_Free := Y + 1; -- Presumed cube
                     end if; -- Rock_Store ((X, Y)) = Round
                  end if; -- Contains (Rock_Store, (X, Y))
               end loop; --  Y in Ordinates range Ordinates'First .. Y_High
            end loop; -- X in Ordinates range Ordinates'First .. X_High
         when others =>
            raise Program_Error with "unimplemented tilt direction";
      end case; -- Direction
   end Tilt;

   function Load (Rock_Store : in Rock_Stores.Map;
                  Y_High : in Ordinates) return Positive is

      Result : Natural := 0;

   begin -- Load
      for R in Iterate (Rock_Store) loop
         if Element (R) = Round then
            Result := @ + Y_High - Key (R).Y + 1;
         end if; --  Element (R) = Round
      end loop; -- R in Iterate (Rock_Store)
      return Result;
   end Load;

   Rock_Store : Rock_Stores.Map;
   X_High, Y_High : Ordinates;

begin -- December_14
   Read_input (Rock_Store, X_High, Y_High);
   Tilt (Rock_Store, North, X_High, Y_High);
   Put_Line ("Part one:" & Load (Rock_Store, Y_High)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_14;
