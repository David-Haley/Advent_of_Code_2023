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

   subtype Steps is Natural range 0 .. 10;

   --  Max_Steps : constant Positive := 3;

   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.X < Right.X or (Left.X = Right.x and Left.Y < Right.Y));

   package Heat_Loss_Stores is new
     Ada.Containers.Ordered_Maps (Coordinates, Heat_Losses);
   use Heat_Loss_Stores;

   type Directions is (North, East, South, West, None);

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
                        X_High, Y_High : in Ordinates;
                        Min_Steps : Steps := 1;
                        Max_Steps : Steps := 3) return Total_Losses is

      type Search_Elements is record
         Coordinate, Previous : Coordinates;
         Heat_Loss : Total_Losses;
         Direction : Directions;
      end record; --Search_Elements

      package History_Maps is new
        Ada.Containers.Ordered_Maps (Coordinates, Total_Losses);
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

         function Valid (Search_Element : in Search_Elements;
                         Visited_Map : in Visited_Maps.Map)
                         return Boolean is

            -- Checks that this position has either not been arrived at from
            -- the previous coordinates or if it has that the heat loss is
            -- lesss.

            Result : Boolean := True;

         begin -- Valid
            if Contains (Visited_Map, Search_Element.Coordinate) and then
              Contains (Visited_Map (Search_Element.Coordinate),
                        Search_Element.Previous) then
               Result := Visited_Map (Search_Element.Coordinate)
                 (Search_Element.Previous) > Search_Element.Heat_Loss;
            end if; -- Contains (Visited_Map, Search_Element.Coordinate) and ...
            -- Fails if this has been visited, from the same approach and
            -- less heat loss.
            return Result;
         end Valid;

         pragma Inline_Always (Valid);

         Next_SE : Search_Elements := Current_SE;
         Enqueue_Next : Boolean;

      begin -- Conditional_Enqueue
         Next_SE.Previous := Current_SE.Coordinate;
         Next_SE.Direction := Direction;
         for Step in Steps range Min_Steps .. Max_Steps loop
            Next_SE.Heat_Loss := Current_SE.Heat_Loss;
            Enqueue_Next := False;
            case Direction is
            when North =>
               if Current_SE.Coordinate.Y - Step >= Ordinates'First then
                  Next_SE.Coordinate.Y := Current_SE.Coordinate.Y - Step;
                  for Y in Ordinates range Next_SE.Coordinate.Y ..
                    Current_SE.Coordinate.Y - 1 loop
                     Next_SE.Heat_Loss := @ +
                       Heat_Loss_Store ((Next_SE.Coordinate.X, Y));
                  end loop; -- Y in Ordinates range Next_SE.Coordinate.Y ...
                  Enqueue_Next := valid (Next_SE, Visited_Map);
               end if; -- Current_SE.Coordinate.Y - Step >= Ordinates'First
            when East =>
               if Current_SE.Coordinate.X + Step <= X_High then
                  Next_SE.Coordinate.X := Current_SE.Coordinate.X + Step;
                  for X in Ordinates range Current_SE.Coordinate.X + 1 ..
                    Next_SE.Coordinate.X loop
                     Next_SE.Heat_Loss := @ +
                       Heat_Loss_Store ((X, Next_SE.Coordinate.Y));
                  end loop; -- X in Ordinates range Current_SE.Coordinate.X + 1
                  Enqueue_Next := valid (Next_SE, Visited_Map);
               end if; -- Current_SE.Coordinate.X + Step <= X_High
            when South =>
               if Current_SE.Coordinate.Y + Step <= Y_High then
                  Next_SE.Coordinate.Y := Current_SE.Coordinate.Y + Step;
                  for Y in Ordinates range Current_SE.Coordinate.Y + 1 ..
                    Next_SE.Coordinate.Y loop
                     Next_SE.Heat_Loss := @ +
                       Heat_Loss_Store ((Next_SE.Coordinate.X, Y));
                  end loop; -- Y in Ordinates range Current_SE.Coordinate.Y + 1
                  Enqueue_Next := valid (Next_SE, Visited_Map);
               end if; -- Current_SE.Coordinate.Y + Step <= Y_High
            when West =>
               if Current_SE.Coordinate.X - Step >= Ordinates'First then
                  Next_SE.Coordinate.X := Current_SE.Coordinate.X - Step;
                  for X in Ordinates range Next_SE.Coordinate.X ..
                    Current_SE.Coordinate.X - 1 loop
                     Next_SE.Heat_Loss := @ +
                       Heat_Loss_Store ((X, Next_SE.Coordinate.Y));
                  end loop; -- X in Ordinates range Next_SE.Coordinate.X ...
                  Enqueue_Next := valid (Next_SE, Visited_Map);
               end if; -- Current_SE.Coordinate.X - Step >= Ordinates'First
            when None =>
               raise Program_Error with "Not a travel direction";
            end case; -- Direction
            if Enqueue_Next then
               if not Contains (Visited_Map, Next_SE.Coordinate) then
                  insert (Visited_Map, Next_SE.Coordinate,
                          History_Maps.Empty_Map);
               end if; -- not Contains (Visited_Map, Next_SE.Coordinate)
               include (Visited_Map (Next_SE.Coordinate), Next_SE.Previous,
                        Next_SE.Heat_Loss);
               Queue.Enqueue (Next_SE);
            end if; -- Enqueue_Next
         end loop; -- Step in Steps range Min_Steps .. Max_Steps
      end  Conditional_Enqueue;

      Current_SE : Search_Elements := ((1, 1), (1, 1), 0, None);
      Visited_Map : Visited_Maps.Map;
      Found : Boolean := False;

   begin -- Least_Heat
      Clear (Visited_Map);
      Queue.Enqueue (Current_SE);
      While Queue.Current_Use > 0 and not Found loop
         Queue.Dequeue (Current_SE);
         Found := Current_SE.Coordinate = (X_High, Y_High);
         if not Found then
            -- Must turn no backtracking
            case Current_SE.Direction is
               when North | South =>
                  Conditional_Enqueue (Current_SE, East, Visited_Map);
                  Conditional_Enqueue (Current_SE, West, Visited_Map);
               when East | West =>
                  Conditional_Enqueue (Current_SE, North, Visited_Map);
                  Conditional_Enqueue (Current_SE, South, Visited_Map);
               when None =>
                  Conditional_Enqueue (Current_SE, North, Visited_Map);
                  Conditional_Enqueue (Current_SE, East, Visited_Map);
                  Conditional_Enqueue (Current_SE, South, Visited_Map);
                  Conditional_Enqueue (Current_SE, West, Visited_Map);
            end case; -- Current_SE.Direction
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
   Put_Line ("Part two:" &
               Least_Heat (Heat_Loss_Store, X_High, Y_High, 4, 10)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_17;
