with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
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

   package Loop_Sets is new Ada.Containers.Ordered_Sets (Coordinates);
   use Loop_Sets;

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
                         Loop_Set : out Loop_Sets.Set) is

      function Next (Pipe : in Pipes.Map;
                     State : in States) return States is

         Next_State : States := State;

      begin -- Next
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
         return Next_State;
      end Next;

      State : States;

   begin -- Pipe_Lemgth
      Clear (Loop_Set);
      State.Coordinate := Start;
      State.Direction := Directions'First;
      loop -- find first exit
         exit when Pipe (Start) (State.Direction);
         State.Direction := Directions'Succ (State.Direction);
         -- will raise an exception if no exit found
      end loop; -- find first exit
      loop -- Take one step
         State := Next (Pipe, State);
         include (Loop_Set, State.Coordinate);
         exit when State.Coordinate = Start;
      end loop; -- Take one step
   end Pipe_Lemgth;

   function Inside_Count (Pipe : in Pipes.map;
                          Loop_Set : in Loop_Sets.Set) return Natural is

      X_Low, Y_Low : Ordinates := Ordinates'Last;
      X_High, Y_High : Ordinates := Ordinates'First;
      Count : Natural := 0;
      Edge_Count : integer;

   begin -- Inside_Count
      for L in Iterate (Loop_Set) loop
         if X_Low > Element (L).X then
            X_Low := Element (L).X;
         end if; -- X_Low > Element (L).X
         if Y_Low > Element (L).Y then
            Y_Low := Element (L).Y;
         end if; -- X_Low > Element (L).Y
         if X_High < Element (L).X then
            X_High := Element (L).X;
         end if; -- X_High < Element (L).X
         if Y_High < Element (L).Y then
            Y_High := Element (L).Y;
         end if; -- X_High < Element (L).Y
      end loop; -- L in Iterate (Loop_Set)
      for Y in Ordinates range Y_Low .. Y_High loop
         for X in Ordinates range X_Low .. X_High loop
            if Contains (Loop_Set, (X, Y)) then
               if Pipe ((X, Y)) (North) then
                  Put ('N');
               elsif Pipe ((X, Y)) (South) then
                  Put ('S');
               elsif Pipe ((X, Y)) (East) or  Pipe ((X, Y)) (West) then
                  Put ('-');
               else
                  Put ('.');
               end if; -- Pipe ((X, Y)) (North)
            end if; -- Contains (Loop_Set, (X, Y))
         end loop; -- X in Ordinates range X_Low .. X_High
         New_Line;
      end loop; -- Y in Ordinates range Y_Low .. Y_High
      for Y in Ordinates range Y_Low .. Y_High loop
         Edge_Count := 0;
         for X in Ordinates range X_Low .. X_High loop
            if Contains (Loop_Set, (X, Y)) then
               if Pipe ((X, Y)) (North) then
                  Edge_Count := Edge_Count + 1;
               elsif Pipe ((X, Y)) (South) then
                  Edge_Count := Edge_Count - 1;
               end if; -- Pipe ((X, Y)) (North)
            end if; -- Contains (Loop_Set, (X, Y))
            Put_Line (X'Img & Y'Img & Edge_Count'Image);
            if Edge_Count mod 2 = 1 and not Contains (Loop_Set, (X, Y)) then
               Count := Count + 1;
               Put_Line (X'Img & Y'Img & Count'Img);
            end if; -- Inside;
         end loop; -- X in Ordinates range X_Low .. X_High
      end loop; -- Y in Ordinates range Y_Low .. Y_High
      return Count;
   end Inside_Count;

   Pipe : Pipes.Map;
   Start : Coordinates;
   Loop_Set : Loop_Sets.Set;

begin -- December_10
   Read_input (Pipe, Start);
   Fix_Start (Pipe, Start);
   Pipe_Lemgth (Pipe, Start, Loop_Set);
   Put_Line ("Part one:" & Positive'Image (Positive (Length (Loop_Set)) / 2));
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" & Inside_Count (Pipe, Loop_Set)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_10;
