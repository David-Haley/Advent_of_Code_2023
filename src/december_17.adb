with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_17 is

   subtype Ordinates is Positive;

   subtype Total_Losses is Natural;

   subtype Heat_Losses is Positive range 1 .. 9;

   Max_Steps : constant Positive := 3;

   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.X < Right.X or (Left.X = Right.x and Left.Y < Right.Y));

   package Heat_Loss_Stores is new
     Ada.Containers.Ordered_Maps (Coordinates, Heat_Losses);
   use Heat_Loss_Stores;

   type Directions is (North, East, South, West);

   procedure Read_Input (Heat_Loss_Store : out Heat_Loss_Stores.Map;
                        X_High, Y_High : out Ordinates) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Y : Ordinates;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_17.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Heat_Loss_Store);
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
            insert (Heat_Loss_Store, (X, Y),
                    Heat_Losses'Value (Slice (Text, X, X)));
         end loop; -- X in Positive range 1 .. Length (Text)
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   function Least_Heat (Heat_Loss_Store : in Heat_Loss_Stores.Map;
                        X_High, Y_High : in Ordinates) return Total_Losses is

      type Search_Elements is record
         Coordinate : Coordinates;
         Heat_Loss : Total_Losses;
         Direction_List : Unbounded_String := Null_Unbounded_String;
      end record; --Search_Elements

      subtype Three_Steps is Unbounded_String;

      package History_Maps is new
        Ada.Containers.Ordered_Maps (Three_Steps, Total_Losses);
      use History_Maps;

      package Visited_Maps is new
        Ada.Containers.Ordered_Maps (Coordinates, History_Maps.Map);
      use Visited_Maps;

      function Get_Priority (Search_Element : Search_Elements)
                             return Total_Losses is
        (Search_Element.Heat_Loss +
           X_High - Search_Element.Coordinate.X +
             Y_High - Search_Element.Coordinate.Y);

      function Before (Left, Right : Total_Losses) return Boolean is
        (Left < Right);

      pragma Inline_Always (Get_Priority, Before);

      package Queue_Interface is new
        Ada.Containers.Synchronized_Queue_Interfaces (Search_Elements);
      use Queue_Interface;

      package Search_Queue is new
        Ada.Containers.Unbounded_Priority_Queues
          (Queue_Interfaces => Queue_Interface,
           Queue_Priority => Total_Losses);
      use Search_Queue;

      Queue : Search_Queue.Queue;

      procedure Conditional_Enqueue (Current_SE : in Search_Elements;
                                     Direction : in Directions;
                                     Visited_Map : in out Visited_Maps.Map) is

         function Encode (Direction : in Directions) return Character is

         begin -- Encode
            case Direction is
               when North => return 'N';
               when South => return 'S';
               when East => return 'E';
               when West => return 'W';
            end case; -- Direction
         end Encode;

         function History_Key (Search_Element : in Search_Elements)
                               return Three_Steps is

            L : Positive := Length (Search_Element.Direction_List);
            Three_Step : Three_Steps;

         begin -- History_Key
            if L >= Max_Steps then
               Three_Step := Unbounded_Slice (Search_Element.Direction_List,
                                          L - Max_Steps + 1, L);
            else
               Three_Step := Search_Element.Direction_List;
            end if; -- L >= Max_Steps
            return Three_Step;
         end History_Key;

         function Backtrack (Search_Element : in Search_Elements)
                             return Boolean is

            L : Positive := Length (Search_Element.Direction_List);

         begin -- Backtrack
            if L < 2 then
               return False;
            else
               return (Element (Search_Element.Direction_List, L) = 'N' and
                         Element (Search_Element.Direction_List, L - 1) = 'S')
                 or (Element (Search_Element.Direction_List, L) = 'S' and
                         Element (Search_Element.Direction_List, L - 1) = 'N')
                 or (Element (Search_Element.Direction_List, L) = 'E' and
                         Element (Search_Element.Direction_List, L - 1) = 'W')
                 or (Element (Search_Element.Direction_List, L) = 'W' and
                         Element (Search_Element.Direction_List, L - 1) = 'E');
            end if; -- L < 2
         end Backtrack;

         pragma Inline_Always (History_Key, Backtrack);

         function Valid (Search_Element : in Search_Elements;
                         Visited_Map : in Visited_Maps.Map)
                         return Boolean is

            -- Checks that the path has not returned to any location previously
            -- traversed and that a forth step with the same X or Y coordinate
            -- is not about to be taken.

            Fail : Boolean := False;
            Count : Natural := 0;
            L : Positive := Length (Search_Element.Direction_List);

         begin -- Valid
            if Contains (Visited_Map, Search_Element.Coordinate) and then
              Contains (Visited_Map (Search_Element.Coordinate),
                        History_Key (Search_Element)) then
               Fail := Visited_Map (Search_Element.Coordinate)
                 (History_Key (Search_Element)) <= Search_Element.Heat_Loss;
            end if; -- Contains (Visited_Map, Search_Element.Coordinate) and ...
            -- Fails if this has been visited, from the same approach and
            -- less heat loss.
            Fail := Fail or Backtrack (Search_Element);
            -- Also fails if there is a direct backtrack.
            for I in Positive range 1 .. Max_Steps loop
               if L - I > 0 then
                  if Element (Search_Element.Direction_List, L) =
                      Element (Search_Element.Direction_List, L - I) then
                     Count := @ + 1;
                  end if; --  Element (Search_Element.Direction_List, L)
               end if; --  L - I > 0
            end loop; -- I in Positive range 1 .. Max_Steps
            return not Fail and Count < Max_Steps;
         end Valid;

         pragma Inline_Always (Valid);

         Next_SE : Search_Elements := Current_SE;
         Enqueue_Next : Boolean := False;

      begin -- Conditional_Enqueue
         Next_SE.Direction_List := @ & Encode (Direction);
         case Direction is
            when North =>
               if Current_SE.Coordinate.Y > Ordinates'First then
                  Next_SE.Coordinate.Y := @ - 1;
                  Next_SE.Heat_Loss := @ + Heat_Loss_Store (Next_SE.Coordinate);
                  Enqueue_Next := valid (Next_SE, Visited_Map);
               end if; -- Current_SE.Coordinate.Y > Ordinates'First
            when East =>
               if Current_SE.Coordinate.X < X_High then
                  Next_SE.Coordinate.X := @ + 1;
                  Next_SE.Heat_Loss := @ + Heat_Loss_Store (Next_SE.Coordinate);
                  Enqueue_Next := valid (Next_SE, Visited_Map);
               end if; -- Current_SE.Coordinate.X < X_High
            when South =>
               if Current_SE.Coordinate.Y < Y_High then
                  Next_SE.Coordinate.Y := @ + 1;
                  Next_SE.Heat_Loss := @ + Heat_Loss_Store (Next_SE.Coordinate);
                  Enqueue_Next := valid (Next_SE, Visited_Map);
               end if; -- Current_SE.Coordinate.Y > Ordinates'First
            when West =>
               if Current_SE.Coordinate.X > Ordinates'First then
                  Next_SE.Coordinate.X := @ - 1;
                  Next_SE.Heat_Loss := @ + Heat_Loss_Store (Next_SE.Coordinate);
                  Enqueue_Next := valid (Next_SE, Visited_Map);
               end if; -- Current_SE.Coordinate.X < X_High
         end case;
         if Enqueue_Next then
            if not Contains (Visited_Map, Next_SE.Coordinate) then
               insert (Visited_Map, Next_SE.Coordinate, History_Maps.Empty_Map);
            end if; -- not Contains (Visited_Map, Next_SE.Coordinate)
            if not Contains (Visited_Map (Next_SE.Coordinate),
                             History_Key (Next_SE)) then
               insert (Visited_Map (Next_SE.Coordinate), History_Key (Next_SE),
                       Next_SE.Heat_Loss);
            else
               Visited_Map (Next_SE.Coordinate) (History_Key (Next_SE)) :=
                 Next_SE.Heat_Loss;
            end if; -- not Contains (Visited_Map (Next_SE.Coordinate), ...
            Queue.Enqueue (Next_SE);
         end if; -- Enqueue_Next
      end  Conditional_Enqueue;

      Current_SE : Search_Elements := ((1, 1), 0, Null_Unbounded_String);
      Visited_Map : Visited_Maps.Map;
      Found : Boolean := False;

   begin -- Least_Heat
      Clear (Visited_Map);
      Queue.Enqueue (Current_SE);
      While Queue.Current_Use > 0 and not Found loop
         Queue.Dequeue (Current_SE);
         Found := Current_SE.Coordinate = (X_High, Y_High);
         if not Found then
            Conditional_Enqueue (Current_SE, North, Visited_Map);
            Conditional_Enqueue (Current_SE, East, Visited_Map);
            Conditional_Enqueue (Current_SE, South, Visited_Map);
            Conditional_Enqueue (Current_SE, West, Visited_Map);
         end if; -- not Found
      end loop; -- Queue.Current_Use > 0
      if Found then
         return Current_SE.Heat_Loss;
      else
         return Total_Losses'Last;
      end if; -- Found
   end Least_Heat;

   Heat_Loss_Store : Heat_Loss_Stores.Map;
   X_High, Y_High : Ordinates;

begin -- December_17
   Read_input (Heat_Loss_Store, X_High, Y_High);
   Put_Line ("Part one:" & Least_Heat (Heat_Loss_Store, X_High, Y_High)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_17;
