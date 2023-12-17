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

   package Round_Sets is new Ada.Containers.Ordered_Sets (Coordinates);
   use Round_Sets;

   subtype Cycles is Positive range Positive'First .. 1000000000;

   package Cycle_Maps is new
     Ada.Containers.Ordered_Maps (Cycles, Round_Sets.Set);

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
               end loop; -- Y in Ordinates range Ordinates'First .. Y_High
            end loop; -- X in Ordinates range Ordinates'First .. X_High
         when South =>
            for X in Ordinates range Ordinates'First .. X_High loop
               First_Free := Y_High;
               for Y in reverse Ordinates range Ordinates'First .. Y_High loop
                  if Contains (Rock_Store, (X, Y)) then
                     if Rock_Store ((X, Y)) = Round then
                        if Y < First_Free then
                           insert (Rock_Store, (X, First_Free), Round);
                           First_Free := @ - 1;
                           Delete (Rock_Store, (X, Y));
                        elsif Y > Ordinates'First then
                           First_Free := Y - 1;
                        else
                           First_Free := Y;
                        end if; -- Y < First_Free
                     elsif Y > Ordinates'First then
                        First_Free := Y - 1; -- Presumed cube
                     end if; -- Rock_Store ((X, Y)) = Round
                  end if; -- Contains (Rock_Store, (X, Y))
               end loop; -- Y in reverse Ordinates range Ordinates'First ...
            end loop; -- X in Ordinates range Ordinates'First .. X_High
         when East =>
            for Y in Ordinates range Ordinates'First .. Y_High loop
               First_Free := X_High;
               for X in reverse Ordinates range Ordinates'First .. X_High loop
                  if Contains (Rock_Store, (X, Y)) then
                     if Rock_Store ((X, Y)) = Round then
                        if X < First_Free then
                           insert (Rock_Store, (First_Free,Y), Round);
                           First_Free := @ - 1;
                           Delete (Rock_Store, (X, Y));
                        elsif X > Ordinates'First then
                           First_Free := X - 1;
                        else
                           First_Free := X;
                        end if; -- X > First_Free
                     elsif X > Ordinates'First then
                        First_Free := X - 1; -- Presumed cube
                     end if; -- Rock_Store ((X, Y)) = Round
                  end if; -- Contains (Rock_Store, (X, Y))
               end loop; -- X in reverse Ordinates range Ordinates'First ...
            end loop; -- Y in Ordinates range Ordinates'First .. Y_High
         when West =>
            for Y in Ordinates range Ordinates'First .. Y_High loop
               First_Free := Ordinates'First;
               for X in Ordinates range Ordinates'First .. X_High loop
                  if Contains (Rock_Store, (X, Y)) then
                     if Rock_Store ((X, Y)) = Round then
                        if X > First_Free then
                           insert (Rock_Store, (First_Free,Y), Round);
                           First_Free := @ + 1;
                           Delete (Rock_Store, (X, Y));
                        else
                           First_Free := X + 1;
                        end if; -- X > First_Free
                     else
                        First_Free := X + 1; -- Presumed cube
                     end if; -- Rock_Store ((X, Y)) = Round
                  end if; -- Contains (Rock_Store, (X, Y))
               end loop; -- X in Ordinates range Ordinates'First .. X_High
            end loop; -- Y in Ordinates range Ordinates'First .. Y_High
      end case; -- Direction
   end Tilt;

   function Load (Round_Set : in Round_Sets.Set;
                  Y_High : in Ordinates) return Positive is

      Result : Natural := 0;

   begin -- Load
      for R in Iterate (Round_Set) loop
         Result := @ + Y_High - Element (R).Y + 1;
      end loop; --  R in Iterate (Round_Sets)
      return Result;
   end Load;

   function Build_Round_Set (Rock_Store : in Rock_Stores.Map)
                             return Round_Sets.Set is

      Result : Round_Sets.Set := Round_Sets.Empty_Set;

   begin -- Build_Round_Set
      for R in Iterate (Rock_Store) loop
         if Element (R) = Round then
            Insert (Result, Key (R));
         end if; -- Element (R) = Round
      end loop; -- Element (R) = Round
      return Result;
   end Build_Round_Set;

   function Spin_Cycle (Rock_Store : in out Rock_Stores.Map;
                        X_High, Y_High : in Ordinates) return Positive is

      -- Has side effect of modifying Rock Store

      use Cycle_Maps;

      procedure Repeated (Rock_Store : in Rock_Stores.Map;
                          Cycle : in Cycles;
                          Cycle_Map : in out Cycle_Maps.Map;
                          Repeat : out Boolean;
                          First_Instance : Out Cycles) is

         Cc : Cycle_Maps.Cursor := First (Cycle_Map);
         Current_Set : Round_Sets.Set := Build_Round_Set (Rock_Store);

      begin -- Repeated
         Repeat := False;
         while not Repeat and  Cc /= Cycle_Maps.No_Element loop
            Repeat := Element (Cc) = Current_Set;
            First_Instance := Key (Cc);
            Next (Cc);
         end loop; -- Cc /= Cycle_Maps.Noelement
         Insert (Cycle_Map, Cycle, Current_Set);
      end Repeated;

      Cycle : Cycles := Cycles'First;
      Cycle_Map : Cycle_Maps.Map := Cycle_Maps.Empty_Map;
      Repeat : Boolean;
      First_Instance, Required_Cycle : Cycles;

   begin -- Spin_Cycle
      loop -- One Spin
         Tilt (Rock_Store, North, X_High, Y_High);
         Tilt (Rock_Store, West, X_High, Y_High);
         Tilt (Rock_Store, South, X_High, Y_High);
         Tilt (Rock_Store, East, X_High, Y_High);
         Repeated (Rock_Store, Cycle, Cycle_Map, Repeat, First_Instance);
         exit when Repeat;
         Cycle := @ + 1;
      end loop; -- One Spin
      Required_Cycle := (Cycles'Last - First_Instance) mod
        (Cycle - First_Instance) + First_Instance;
      return Load (Cycle_Map (Required_Cycle), Y_High);
   end Spin_Cycle; -- One Spin

   Rock_Store : Rock_Stores.Map;
   X_High, Y_High : Ordinates;

begin -- December_14
   Read_input (Rock_Store, X_High, Y_High);
   Tilt (Rock_Store, North, X_High, Y_High);
   Put_Line ("Part one:" & Load (Build_Round_Set (Rock_Store), Y_High)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Read_input (Rock_Store, X_High, Y_High);
   Put_Line ("Part two:" & Spin_Cycle (Rock_Store, X_High, Y_High)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_14;
