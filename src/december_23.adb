with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_23 is

   subtype Ordinates is Positive;

   type Paths is (Flat, North, East, South, West);

   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.X < Right.X or (Left.X = Right.x and Left.Y < Right.Y));

   pragma Inline_Always ("<");

   package Island_Maps is new
     Ada.Containers.Ordered_Maps (Coordinates, Paths);
   use Island_Maps;

   subtype Directions is Paths range North .. West;
   -- Allows direct matching between direction and path;

   subtype Steps is Natural;

   type Junctions is record
      Coordinate : Coordinates;
      Step : Steps;
   end record; -- Junction;

   package Junction_Lists is new Ada.Containers.Doubly_Linked_Lists (Junctions);
   use Junction_Lists;

   package Junction_Maps is new
     Ada.Containers.Ordered_Maps (Coordinates, Junction_Lists.List);
   use Junction_Maps;

   package Visited_Sets is new Ada.Containers.Ordered_Sets (Coordinates);
   use Visited_Sets;

   procedure Read_Input (Island_Map : out Island_Maps.Map;
                         X_Low, Y_Low, X_High, Y_High : out Ordinates) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Y : Ordinates;
      Path : Paths;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_23.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Island_Map);
      X_Low := Ordinates'Last;
      Y_Low := Ordinates'Last;
      X_High := Ordinates'First;
      Y_High := Ordinates'First;
      while not End_Of_File (Input_File) loop
         Y := Ordinates (Line (Input_File));
         Get_Line (Input_File, Text);
         for X in Positive range 1 .. Length (Text) loop
            if Element (Text, X) /= '#' then
               if X_Low > X then
                  X_Low := X;
               end if; -- X_Low > X
               if X_High < X then
                  X_High := X;
               end if; -- X_High < X
               if Y_Low > Y then
                  Y_Low := Y;
               end if; -- Y_Low > Y
               if Y_High < Y then
                  Y_High := Y;
               end if; -- Y_High < Y
               case Element (Text, X) is
               when '.' => Path := Flat;
               when '^' => Path := North; -- Slope
               when '>' => Path := East;
               when 'v' => Path := South;
               when '<' => Path := West;
               when others => raise Program_Error with
                    "unexpected character '" & Element (Text, X) & "'";
               end case; -- Element (Text, X)
               insert (Island_Map, (X, Y), Path);
            end if; -- Element (Text, X) /= '#'
         end loop; -- X in Positive range 1 .. Length (Text)
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   procedure Build_Junctions (Island_Map : in Island_Maps.Map;
                              X_Low, Y_Low, X_High, Y_High : in Ordinates;
                              Junction_Map : out Junction_Maps.Map;
                              Part_2 : Boolean := False) is

      package Options is new Ada.Containers.Ordered_Sets (Directions);
      use Options;

      type Search_Elements is record
         Coordinate : Coordinates;
         Step : Steps;
      end record; --Search_Elements

      package Queue_Interface is new
        Ada.Containers.Synchronized_Queue_Interfaces (Search_Elements);
      use Queue_Interface;

      package Search_Queue is new
        Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interface);
      use Search_Queue;

      procedure Find_Junctions (Island_Map : in Island_Maps.Map;
                                Junction_Map : out Junction_Maps.Map) is

         Path_Count : Natural;

      begin -- Find_Junctions
         Clear (Junction_Map);
         for I in Iterate (Island_Map) loop
            Path_Count := 0;
            if Key (I).X > Ordinates'First and then
              Contains (Island_Map, (Key (I).X - 1, Key (I).Y)) then
               Path_Count := @ + 1;
            end if; -- Key (I).X > Ordinates'First and then ...
            if Contains (Island_Map, (Key (I).X + 1, Key (I).Y)) then
               Path_Count := @ + 1;
            end if; -- Contains (Island_Map, (Key (I).X + 1, Key (I).Y))
            if Key (I).Y > Ordinates'First and then
              Contains (Island_Map, (Key (I).X, Key (I).Y - 1)) then
               Path_Count := @ + 1;
            end if; -- Key (I).Y > Ordinates'First and then ...
            if Contains (Island_Map, (Key (I).X, Key (I).Y + 1)) then
               Path_Count := @ + 1;
            end if; -- Contains (Island_Map, (Key (I).X, Key (I).Y + 1))
            if Path_Count >= 3 then
               insert (Junction_Map, Key (I), Junction_Lists.Empty_List);
            end if; -- Path_Count >= 3
         end loop; -- I in Iterate (Island_Map);
         -- Searches also start and end at the entrance and exit
         insert (Junction_Map, (X_Low, Y_Low),
                 Junction_Lists.Empty_List);
         insert (Junction_Map, (X_High, Y_High),
                 Junction_Lists.Empty_List);
      end Find_Junctions;

      procedure Find_Options (Current_SE : in Search_Elements;
                              Island_Map : in Island_Maps.Map;
                              Visited_Set : in Visited_Sets.Set;
                              Option : out Options.Set;
                              Part_2 : in Boolean) is

         Test : Coordinates;
         Valid : Boolean;

      begin -- Find_Options
         Clear (Option);
         for Direction in Directions loop
            Valid := True;
            Test := Current_SE.Coordinate;
            case Direction is
            when North =>
               if Current_SE.Coordinate.Y > Ordinates'First then
                  Test.Y := @ - 1;
               else
                  Valid := False;
               end if; -- Current_SE.Coordinate.Y > Ordinates'First
            when East =>
               if Current_SE.Coordinate.X < X_High then
                  Test.X := @ + 1;
               else
                  Valid := False;
               end if; -- Current_SE.Coordinate.X < X_High
            when South =>
               if Current_SE.Coordinate.Y < Y_High then
                  Test.Y := @ + 1;
               else
                  Valid := False;
               end if; -- Current_SE.Coordinate.Y > Ordinates'First
            when West =>
               if Current_SE.Coordinate.X > Ordinates'First then
                  Test.X := @ - 1;
               else
                  Valid := False;
               end if; -- Current_SE.Coordinate.X < X_High
            end case; -- Direction
            if Valid and then (Contains (Island_Map, Test) and
                                 not Contains (Visited_Set, Test)) and then
              (Part_2 or else Island_Map (Test) = Flat or else
               Island_Map (Test) = Direction)
            then
               insert (Option, Direction);
            end if; -- Valid and then (Contains (Island_Map, Test) and ...
         end loop; -- Direction in Directions
      end Find_Options;

      Option : Options.Set;
      Queue : Search_Queue.Queue;
      Current_SE, Test : Search_Elements;
      Visited_Set : Visited_Sets.Set;

   begin -- Build_Junctions
      Clear (Junction_Map);
      Find_Junctions (Island_Map, Junction_Map);
      for J in Iterate (Junction_Map) loop
         Clear (Visited_Set);
         Current_SE.Coordinate := Key (J);
         Current_SE.Step := 0;
         Insert (Visited_Set, Key (J));
         Queue.Enqueue (Current_SE);
         While Queue.Current_Use > 0 loop
            Queue.Dequeue (Current_SE);
            if Current_SE.Coordinate /= Key (J) and then
              Contains (Junction_Map, Current_SE.Coordinate) then
               Append (Junction_Map (Key (J)),
                       (Current_SE.Coordinate, Current_SE.Step));
               -- Search terminates when a junction is found
            else
               Find_Options (Current_SE, Island_Map, Visited_Set, Option,
                             Part_2);
               Test.Step := Current_SE.Step + 1;
               for O in Iterate (Option) loop
                  Test.Coordinate := Current_SE.Coordinate;
                  case Element (O) is
                  when North =>
                     Test.Coordinate.Y := @ - 1;
                  when East =>
                     Test.Coordinate.X := @ + 1;
                  when South =>
                     Test.Coordinate.Y := @ + 1;
                  when West =>
                     Test.Coordinate.X := @ - 1;
                  end case;
                  Include (Visited_Set, Test.Coordinate);
                  Queue.Enqueue (Test);
               end loop; -- O in Iterate (Option)
            end if; -- Current_SE.Coordinate /= Key (J) and then ...
         end loop; -- Queue.Current_Use > 0
      end loop; -- J in Iterate (Junction_Map)
   end Build_Junctions;

   function Most_Scenic (Junction_Map : in Junction_Maps.Map;
                         X_Low, Y_Low, X_High, Y_High : in Ordinates)
                         return Natural is

      procedure Find (Junction_Map : in Junction_Maps.Map;
                      Start : in Coordinates;
                      X_High, Y_High : in Ordinates;
                      Length : in Natural;
                      Visited_Set : in out Visited_Sets.Set;
                      Longest : in out Natural) is

      begin -- Find
         if Start = (X_High, Y_High) then
            if Longest < Length then
               Longest := Length;
            end if; -- Longest < Length
         else
            for S in Iterate (Junction_Map (Start)) loop
               if not Contains (Visited_Set, Element (S).Coordinate) then
                  Include (Visited_Set, Element (S).Coordinate);
                  Find (Junction_Map, Element (S).Coordinate, X_High, Y_High,
                        Length + Element (S).Step, Visited_Set, Longest);
                  Exclude (Visited_Set, Element (S).Coordinate);
               end if; -- not Contains (Visited_Set, Element (S).Coordinate)
            end loop; -- S in Iterate (Junction_Map (Start))
         end if; -- Start = (X_High, Y_High)
      end Find;

      Longest : Natural := 0;
      Visited_Set : Visited_Sets.Set := To_Set ((X_Low, Y_Low));

   begin -- Most_Scenic
      Find (Junction_Map, (X_Low, Y_Low), X_High, Y_High, 0, Visited_Set,
            Longest);
      return Longest;
   end Most_Scenic;

   Island_Map : Island_Maps.Map;
   X_Low, Y_Low, X_High, Y_High : Ordinates;
   Junction_Map : Junction_Maps.Map;

begin -- December_23
   Read_input (Island_Map, X_Low, Y_Low, X_High, Y_High);
   Build_Junctions (Island_Map, X_Low, Y_Low, X_High, Y_High, Junction_Map);
   Put_Line ("Part one:" &
               Most_Scenic (Junction_Map, X_Low, Y_Low, X_High, Y_High)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Build_Junctions (Island_Map, X_Low, Y_Low, X_High, Y_High, Junction_Map,
                    True);
   Put_Line ("Part two:" &
               Most_Scenic (Junction_Map, X_Low, Y_Low, X_High, Y_High)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_23;
