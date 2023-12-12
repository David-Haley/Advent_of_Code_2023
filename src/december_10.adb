with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_10 is

   subtype Ordinates is Natural;
   -- allows 0 to be off map

   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   type Directions is (North, South, East, West);

   type States is record
      Coordinate : Coordinates;
      Direction : Directions;
   end record; -- States

   type Pipe_Elements is array (Directions) of Boolean;
   -- True indicates entrance or exit possible

   function "<" (Left, Right : Coordinates) return Boolean is
      (Left.X < Right.X or (Left.X = Right.x and Left.Y < Right.Y));

   package Pipes is new
     Ada.Containers.Ordered_Maps (Coordinates, Pipe_Elements);
   use Pipes;

   procedure Read_input (Pipe : out Pipes.Map;
                         Start : out Coordinates) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Pipe_Element : Pipe_Elements;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_10.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Pipe);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         for X in Positive range 1 .. Length (Text) loop
            case Element (Text, X) is
               when '|' =>
                  Pipe_Element := (True, True, False, False);
                  include (Pipe, (X, Ordinates (Line (Input_File) - 1)),
                           Pipe_Element);
               when '-' =>
                  Pipe_Element := (False, False, True, True);
                  include (Pipe, (X, Ordinates (Line (Input_File) - 1)),
                           Pipe_Element);
               when 'L' =>
                  Pipe_Element := (True, False, True, False);
                  include (Pipe, (X, Ordinates (Line (Input_File) - 1)),
                           Pipe_Element);
               when 'J' =>
                  Pipe_Element := (True, False, False, True);
                  include (Pipe, (X, Ordinates (Line (Input_File) - 1)),
                           Pipe_Element);
               when '7' =>
                  Pipe_Element := (False, True, False, True);
                  include (Pipe, (X, Ordinates (Line (Input_File) - 1)),
                           Pipe_Element);
               when 'F' =>
                  Pipe_Element := (False, True, True, False);
                  include (Pipe, (X, Ordinates (Line (Input_File) - 1)),
                           Pipe_Element);
               when '.' =>
                  Null; -- no pipe here
               when 'S' =>
                  Pipe_Element := (others => False);
                  -- Determine connections after all map is Read
                  include (Pipe, (X, Ordinates (Line (Input_File) - 1)),
                           Pipe_Element);
                  Start := (X, Ordinates (Line (Input_File) - 1));
               when others =>
                  raise Program_Error with "illegal character '" &
                    Element (Text, X) & "'";
            end case; -- Element (Text, X)
         end loop; -- X in Positive range 1 .. Length (Text)
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   procedure Fix_Start (Pipe : in out Pipes.Map;
                        Start : in Coordinates) is

      Direction_Set : Boolean := False;
      C : Coordinates;

   begin -- Fix_Start
      C := Start;
      C.X := C.X - 1;
      if Contains (Pipe, C) then
         Pipe (Start) (West) := Pipe (C) (East);
      end if; -- Contains (Pipe, C)
      C := Start;
      C.X := C.X + 1;
      if Contains (Pipe, C) then
         Pipe (Start) (East) := Pipe (C) (West);
      end if; -- Contains (Pipe, C)
      C := Start;
      C.Y := C.Y - 1;
      if Contains (Pipe, C) then
         Pipe (Start) (North) := Pipe (C) (South);
      end if; -- Contains (Pipe, C)
      C := Start;
      C.Y := C.Y + 1;
      if Contains (Pipe, C) then
         Pipe (Start) (South) := Pipe (C) (North);
      end if; -- Contains (Pipe, C)
   end Fix_Start;

   procedure Pipe_Lemgth (Pipe : in Pipes.Map;
                          Start : in Coordinates;
                          Loop_Map : out Pipes.Map) is

      -- Has the side effect of creating a map of the loop

      procedure Update (State : in States;
                        Loop_Map : in out Pipes.Map) is

         Empty_Direction : constant Pipe_Elements := (others => False);

      begin -- Update
         If not Contains (Loop_Map, State.Coordinate) then
            include (Loop_Map, State.Coordinate, Empty_Direction);
         end if; -- not Contains (Loop_Map, State.Coordinate)
         Loop_Map (State.Coordinate) (State.Direction) := True;
      end Update;

      procedure Next (Pipe : in Pipes.Map;
                      State : in out States;
                      Loop_Map : in out Pipes.Map) is

         Next_State : States := State;

      begin -- Next
         Update (State, Loop_Map);
         if not Pipe (Next_State.Coordinate) ( Next_State.Direction) then
            case Next_State.Direction is
            when North | South =>
               if Pipe (Next_State.Coordinate) (East) then
                  Next_State.Direction := East;
               elsif Pipe (Next_State.Coordinate) (West) then
                  Next_State.Direction := West;
               else
                  raise Program_Error with "No forward route " &
                    Next_State'Img;
               end if; -- Pipe (Next_State.Coordinate) (East)
            when East | West =>
               if Pipe (Next_State.Coordinate) (North) then
                  Next_State.Direction := North;
               elsif Pipe (Next_State.Coordinate) (South) then
                  Next_State.Direction := South;
               else
                  raise Program_Error with "No forward route " &
                    Next_State'Img;
               end if; -- Pipe (Next_State.Coordinate) (North)
            end case; -- Next_State.Direction
         end if; -- not Pipe (Next_State.Coordinate) ( Next_State.Direction)
         Update ((State.Coordinate, Next_State.Direction), Loop_Map);
         case Next_State.Direction is
            when North =>
               Next_State.Coordinate.Y := Next_State.Coordinate.Y - 1;
               if not Pipe (Next_State.Coordinate) (South) then
                  raise Program_Error with "Pipe blocked (" &
                    Next_State.Coordinate.X'Img & "," &
                    Next_State.Coordinate.Y'Img & ") South";
               end if; -- not Pipe (Next_State.Coordinate) (South)
            when South =>
               Next_State.Coordinate.Y := Next_State.Coordinate.Y + 1;
               if not Pipe (Next_State.Coordinate) (North) then
                  raise Program_Error with "Pipe blocked (" &
                    Next_State.Coordinate.X'Img & "," &
                    Next_State.Coordinate.Y'Img & ") North";
               end if; -- not Pipe (Next_State.Coordinate) (North)
            when East =>
               Next_State.Coordinate.X := Next_State.Coordinate.X + 1;
               if not Pipe (Next_State.Coordinate) (West) then
                  raise Program_Error with "Pipe blocked (" &
                    Next_State.Coordinate.X'Img & "," &
                    Next_State.Coordinate.Y'Img & ") West";
               end if; -- not Pipe (Next_State.Coordinate) (West)
            when West =>
               Next_State.Coordinate.X := Next_State.Coordinate.X - 1;
               if not Pipe (Next_State.Coordinate) (East) then
                  raise Program_Error with "Pipe blocked (" &
                    Next_State.Coordinate.X'Img & "," &
                    Next_State.Coordinate.Y'Img & ") East";
               end if; -- not Pipe (Next_State.Coordinate) (East)
         end case; -- Next_State.Direction
         Update (Next_State, Loop_Map);
         State := Next_State;
      end Next;

      State : States;

   begin -- Pipe_Lemgth
      Clear (Loop_Map);
      State.Coordinate := Start;
      State.Direction := Directions'First;
      loop -- find first exit
         exit when Pipe (Start) (State.Direction);
         State.Direction := Directions'Succ (State.Direction);
         -- will raise an exception if no exit found
      end loop; -- find first exit
      loop -- Take one step
         Next (Pipe, State, Loop_Map);
         exit when State.Coordinate = Start;
      end loop; -- Take one step
   end Pipe_Lemgth;

   function Inside_Count (Pipe : in Pipes.map;
                          Loop_Map : in Pipes.Map) return Natural is

      -- To be inside loop, the count of crossings of the loop in a row has to
      -- be odd.

      X_Low, Y_Low : Ordinates := Ordinates'Last;
      X_High, Y_High : Ordinates := Ordinates'First;
      Count : Natural := 0;
      Edge_Count : integer;
      In_Loop : Boolean;
      Previous_NS : Directions;

   begin -- Inside_Count
      -- Only consider the bounding box of the loop
      for L in Iterate (Loop_Map) loop
         if X_Low > Key (L).X then
            X_Low := Key (L).X;
         end if; -- X_Low > Key (L).X
         if Y_Low > Key (L).Y then
            Y_Low := Key (L).Y;
         end if; -- X_Low > Key (L).Y
         if X_High < Key (L).X then
            X_High := Key (L).X;
         end if; -- X_High < Key (L).X
         if Y_High < Key (L).Y then
            Y_High := Key (L).Y;
         end if; -- X_High < Key (L).Y
      end loop; -- L in Iterate (Loop_Map)
      for Y in Ordinates range Y_Low .. Y_High loop
         Edge_Count := 0;
         Previous_NS := East; -- has to be anything other than North or South
         In_Loop := False;
         for X in Ordinates range X_Low .. X_High loop
            if Contains (Loop_Map, (X, Y)) then
               -- Don't count twice for entrances and exits from a row in the
               -- same direction
               if Loop_Map ((X, Y)) (North) and
                 not (Previous_NS = North and In_Loop) then
                  Edge_Count := Edge_Count + 1;
               elsif Loop_Map ((X, Y)) (South) and
                 not (Previous_NS = South and In_Loop) then
                  Edge_Count := Edge_Count - 1;
               end if; -- Pipe ((X, Y)) (North)
               -- Set In_loop False if the row has been exited in the same
               -- direction as it was entered
               if In_Loop and then
                 ((Loop_Map ((X, Y)) (North) and Previous_NS = North) or
                    (Loop_Map ((X, Y)) (South) and Previous_NS = South)) then
                  In_Loop := False;
                  Previous_NS := East;
                  -- Anything other than North or South but it may not be
                  -- essential to make it invalid here;
               end if; -- In_Loop and then
               -- In_Loop becomes true when a bend is in the current row and
                       -- remains true whilst there is a connection in that row.
               In_Loop := (In_loop or Loop_Map ((X, Y)) (North) or
                             Loop_Map ((X, Y)) (South)) and
                 (Loop_Map ((X, Y)) (East) or Loop_Map ((X, Y)) (West));
               if Loop_Map ((X, Y)) (North) then
                  Previous_NS := North;
               elsif Loop_Map ((X, Y)) (South) then
                  Previous_NS := South;
               end if; -- Loop_Map ((X, Y)) (North)
            end if; -- Contains (Loop_Map, (X, Y))
            if Edge_Count mod 2 = 1 and not Contains (Loop_Map, (X, Y)) then
               Count := Count + 1;
            end if; -- Edge_Count mod 2 = 1 and not Contains (Loop_Map, (X, Y))
         end loop; -- X in Ordinates range X_Low .. X_High
      end loop; -- Y in Ordinates range Y_Low .. Y_High
      return Count;
   end Inside_Count;

   Pipe : Pipes.Map;
   Start : Coordinates;
   Loop_Map : Pipes.Map;
   -- Used to record elements in the loop and the direction treversed.

begin -- December_10
   Read_input (Pipe, Start);
   Fix_Start (Pipe, Start);
   Pipe_Lemgth (Pipe, Start, Loop_Map);
   Put_Line ("Part one:" & Positive'Image (Positive (Length (Loop_Map)) / 2));
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" & Inside_Count (Pipe, Loop_Map)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_10;
