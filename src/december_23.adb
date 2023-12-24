with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_23 is

   subtype Ordinates is Natural; -- Allows for doing inclusion test without
   -- testing coordinates first;

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

   function Most_Senic (Island_Map : in Island_Maps.Map;
                        X_Low, Y_Low, X_High, Y_High : in Ordinates;
                        Part_2 : Boolean := False)
                        return Steps is

      package Visited_Sets is new Ada.Containers.Ordered_Sets (Coordinates);
      use Visited_Sets;

      type Search_Elements is record
         Coordinate : Coordinates;
         Step : Steps;
         Visited_Set : Visited_Sets.Set := Visited_Sets.Empty_Set;
      end record; --Search_Elements

      package Queue_Interface is new
        Ada.Containers.Synchronized_Queue_Interfaces (Search_Elements);
      use Queue_Interface;

      package Search_Queue is new
        Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interface);
      use Search_Queue;

      Queue : Search_Queue.Queue;

      procedure Conditional_Enqueue (Current_SE : in Search_Elements;
                                     Direction : in Directions;
                                     Island_Map : in Island_Maps.Map) is

         Next_SE : Search_Elements :=
           (Current_SE.Coordinate, Current_SE.Step + 1,
            copy (Current_SE.Visited_Set));

      begin -- Conditional_Enqueue
         case Direction is
         when North =>
            if Current_SE.Coordinate.Y > Ordinates'First then
               Next_SE.Coordinate.Y := @ - 1;
            end if; -- Current_SE.Coordinate.Y > Ordinates'First
         when East =>
            if Current_SE.Coordinate.X < X_High then
               Next_SE.Coordinate.X := @ + 1;
            end if; -- Current_SE.Coordinate.X < X_High
         when South =>
            if Current_SE.Coordinate.Y < Y_High then
               Next_SE.Coordinate.Y := @ + 1;
            end if; -- Current_SE.Coordinate.Y > Ordinates'First
         when West =>
            if Current_SE.Coordinate.X > Ordinates'First then
               Next_SE.Coordinate.X := @ - 1;
            end if; -- Current_SE.Coordinate.X < X_High
         end case;
         if Contains (Island_Map, Next_SE.Coordinate) and then
           not Contains (Next_SE.Visited_Set, Next_SE.Coordinate) then
            insert (Next_SE.Visited_Set, Next_SE.Coordinate);
            Queue.Enqueue (Next_SE);
         end if; -- not Contains (Visited_Map, Next_SE.Coordinate).
      end Conditional_Enqueue;

      pragma Inline_Always (Conditional_Enqueue);

      procedure Put (Island_Map : in Island_Maps.Map;
                     Visited_Set : Visited_Sets.Set;
                     X_Low, Y_Low, X_High, Y_High : in Ordinates) is

      begin -- Put
         for Y in Natural range Y_Low .. Y_High loop
            for X in Natural range X_Low .. X_High loop
               if Contains (Visited_Set, (X, Y)) then
                  Put ('O');
               elsif Contains (Island_Map, (X, Y)) then
                  case Island_Map ((X, Y)) is
                     when Flat => Put ('.');
                     when North => Put ('^');
                     when East => Put ('>');
                     when South => Put ('V');
                     when West => Put ('<');
                  end case;
               else
                  Put ('#');
               end if;
            end loop;
            New_Line;
         end loop;
         New_Line;
      end Put;

      Current_SE : Search_Elements := ((X_Low, Y_Low), 0,
                                       Visited_Sets.Empty_Set);
      Result : Steps := 0;

   begin -- Most_Senic
      Insert (Current_SE.Visited_Set, Current_SE.Coordinate);
      Queue.Enqueue (Current_SE);
      While Queue.Current_Use > 0 loop
         Queue.Dequeue (Current_SE);
         if Current_SE.Coordinate = (X_High, Y_High) then
            Put_Line (Current_SE.Step'Img);
            if Current_SE.Step > Result then
               Result := Current_SE.Step;
               Put (Island_Map, Current_SE.Visited_Set, X_Low, Y_Low, X_High, Y_High);
            end if; --Current_SE.Step > Result
         elsif Island_Map (Current_SE.Coordinate) = Flat or Part_2 then
            Conditional_Enqueue (Current_SE, North, Island_Map);
            Conditional_Enqueue (Current_SE, East, Island_Map);
            Conditional_Enqueue (Current_SE, South, Island_Map);
            Conditional_Enqueue (Current_SE, West, Island_Map);
         else
            Conditional_Enqueue (Current_SE, Island_Map (Current_SE.Coordinate),
                                 Island_Map);
         end if; -- Island_Map (Current_SE.Coordinate)
      end loop; -- Queue.Current_Use > 0
      Return Result;
   end Most_Senic;

   Island_Map : Island_Maps.Map;
   X_Low, Y_Low, X_High, Y_High : Ordinates;

begin -- December_23
   Read_input (Island_Map, X_Low, Y_Low, X_High, Y_High);
   Put_Line ("Part one:" &
               Most_Senic (Island_Map, X_Low, Y_Low, X_High, Y_High)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" &
               Most_Senic (Island_Map, X_Low, Y_Low, X_High, Y_High, True)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_23;
