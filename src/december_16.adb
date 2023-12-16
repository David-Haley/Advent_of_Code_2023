with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_16 is

   subtype Ordinates is Positive;

   type Optics is (Mirror_NW_SE, Mirror_NE_SW, Splitter_NS, Splitter_EW);

   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.X < Right.X or (Left.X = Right.x and Left.Y < Right.Y));

   package Optical_Stores is new
     Ada.Containers.Ordered_Maps (Coordinates, Optics);
   use Optical_Stores;

   type Directions is (North, West, South, East);

   package Tile_Sets is new Ada.Containers.Ordered_Sets (Coordinates);
   use Tile_Sets;

   procedure Read_Input (Optical_Store : out Optical_Stores.Map;
                        X_High, Y_High : out Ordinates) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Y : Ordinates;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_16.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Optical_Store);
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
            if Element (Text, X) = '/' then
               insert (Optical_Store, (X, Y), Mirror_NE_SW);
            elsif  Element (Text, X) = '\' then
               insert (Optical_Store, (X, Y), Mirror_NW_SE);
            elsif Element (Text, X) = '|' then
               insert (Optical_Store, (X, Y), Splitter_NS);
            elsif  Element (Text, X) = '-' then
               insert (Optical_Store, (X, Y), Splitter_EW);
            end if; -- Element (Text, X) = '/'
         end loop; -- X in Positive range 1 .. Length (Text)
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   procedure Ray_Trace (Optical_Store : in Optical_Stores.Map;
                        X_High, Y_High : in Ordinates;
                        Start_X, Start_Y : in Ordinates;
                        Start_Direction : in Directions;
                        Tile_Set : out Tile_Sets.Set) is

      type Search_Elements is record
         Coordinate : Coordinates;
         Direction : Directions;
      end record; --Search_Elements

      package Queue_Interface is new
        Ada.Containers.Synchronized_Queue_Interfaces (Search_Elements);
      use Queue_Interface;

      package Search_Queue is new
        Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interface);
      use Search_Queue;

      function "<" (Left, Right : Search_Elements) return boolean is
        (Left.Direction < Right.Direction or (Left.Direction = Right.Direction
         and Left.Coordinate < Right.Coordinate));

      package Search_Sets is new Ada.Containers.Ordered_Sets (Search_Elements);
      use Search_Sets;

      Queue : Search_Queue.Queue;

      procedure Conditional_Enqueue (Search_Element : in Search_Elements;
                                   Search_Set : in Out Search_Sets.Set) is

         -- Don't enqueue search elements that are identical to ones previously
         -- enqueued

      begin -- Conditional_Enqueue
         if not Contains (Search_Set, Search_Element) then
            Insert (Search_Set, Search_Element);
            Queue.Enqueue (Search_Element);
         end if; -- not Contains (Search.Ste, Search_Element)
      end  Conditional_Enqueue;

      Current_SE : Search_Elements := ((Start_X, Start_Y), Start_Direction);
      Next_SE : Search_Elements;
      Search_Set : Search_Sets.Set := Search_Sets.Empty_Set;

   begin -- Ray_Trace
      Clear (Tile_Set);
      Queue.Enqueue (Current_SE);
      While Queue.Current_Use > 0 loop
         Queue.Dequeue (Current_SE);
         Include (Tile_Set, Current_SE.Coordinate);
         Next_SE := Current_SE;
         if Contains (Optical_Store, Current_SE.Coordinate) then
            case Optical_Store (Current_SE.Coordinate) is
               when Mirror_NW_SE =>
                  case Current_SE.Direction is
                  when North =>
                     Next_SE.Direction := West;
                     if Current_SE.Coordinate.X > Ordinates'First then
                        Next_SE.Coordinate.X := Current_SE.Coordinate.X - 1;
                        Conditional_Enqueue (Next_SE, Search_Set);
                     end if; -- Current_SE.Coordinate.X > Ordinates'First
                  when South =>
                     Next_SE.Direction := East;
                     if Current_SE.Coordinate.X < X_High then
                        Next_SE.Coordinate.X := Current_SE.Coordinate.X + 1;
                        Conditional_Enqueue (Next_SE, Search_Set);
                     end if; -- Current_SE.Coordinate.X > Ordinates'First
                  when East =>
                     Next_SE.Direction := South;
                     if Current_SE.Coordinate.Y < Y_High then
                        Next_SE.Coordinate.Y := Current_SE.Coordinate.Y + 1;
                        Conditional_Enqueue (Next_SE, Search_Set);
                     end if; -- Current_SE.Coordinate.Y < Y_High
                  when West =>
                     Next_SE.Direction := North;
                     if Current_SE.Coordinate.Y > Ordinates'First then
                        Next_SE.Coordinate.Y := Current_SE.Coordinate.Y - 1;
                        Conditional_Enqueue (Next_SE, Search_Set);
                     end if; -- Current_SE.Coordinate.Y > Ordinates'First
                  end case; -- Current_SE.Direction
               when Mirror_NE_SW =>
                  case Current_SE.Direction is
                  when North =>
                     Next_SE.Direction := East;
                     if Current_SE.Coordinate.X < X_High then
                        Next_SE.Coordinate.X := Current_SE.Coordinate.X + 1;
                        Conditional_Enqueue (Next_SE, Search_Set);
                     end if; -- Current_SE.Coordinate.X > Ordinates'First
                  when South =>
                     Next_SE.Direction := West;
                     if Current_SE.Coordinate.X > Ordinates'First then
                        Next_SE.Coordinate.X := Current_SE.Coordinate.X - 1;
                        Conditional_Enqueue (Next_SE, Search_Set);
                     end if; -- Current_SE.Coordinate.X > Ordinates'First
                  when East =>
                     Next_SE.Direction := North;
                     if Current_SE.Coordinate.Y > Ordinates'First then
                        Next_SE.Coordinate.Y := Current_SE.Coordinate.Y - 1;
                        Conditional_Enqueue (Next_SE, Search_Set);
                     end if; -- Current_SE.Coordinate.Y > Ordinates'First
                  when West =>
                     Next_SE.Direction := South;
                     if Current_SE.Coordinate.Y < Y_High then
                        Next_SE.Coordinate.Y := Current_SE.Coordinate.Y + 1;
                        Conditional_Enqueue (Next_SE, Search_Set);
                     end if; -- Current_SE.Coordinate.Y < Y_High
                  end case; -- Current_SE.Direction
               when Splitter_NS =>
                  case Current_SE.Direction is
                  when North =>
                     if Current_SE.Coordinate.Y > Ordinates'First then
                        Next_SE.Coordinate.Y := Current_SE.Coordinate.Y - 1;
                        Conditional_Enqueue (Next_SE, Search_Set);
                     end if; -- Current_SE.Coordinate.Y > Ordinates'First
                  when South =>
                     if Current_SE.Coordinate.Y < Y_High then
                        Next_SE.Coordinate.Y := Current_SE.Coordinate.Y + 1;
                        Conditional_Enqueue (Next_SE, Search_Set);
                     end if; -- Current_SE.Coordinate.Y < Y_High
                  when East | West =>
                     Next_SE.Direction := North;
                     if Current_SE.Coordinate.Y > Ordinates'First then
                        Next_SE.Coordinate.Y := Current_SE.Coordinate.Y - 1;
                        Conditional_Enqueue (Next_SE, Search_Set);
                     end if; -- Current_SE.Coordinate.Y > Ordinates'First
                     Next_SE.Direction := South;
                     if Current_SE.Coordinate.Y < Y_High then
                        Next_SE.Coordinate.Y := Current_SE.Coordinate.Y + 1;
                        Conditional_Enqueue (Next_SE, Search_Set);
                     end if; -- Current_SE.Coordinate.Y > Ordinates'First
                  end case; -- Current_SE.Direction
               when Splitter_EW =>
                  case Current_SE.Direction is
                  when North | South =>
                     Next_SE.Direction := East;
                     if Current_SE.Coordinate.X < X_High then
                        Next_SE.Coordinate.X := Current_SE.Coordinate.X + 1;
                        Conditional_Enqueue (Next_SE, Search_Set);
                     end if; -- Current_SE.Coordinate.X > Ordinates'First
                     Next_SE.Direction := West;
                     if Current_SE.Coordinate.X > Ordinates'First then
                        Next_SE.Coordinate.X := Current_SE.Coordinate.X - 1;
                        Conditional_Enqueue (Next_SE, Search_Set);
                     end if; -- Current_SE.Coordinate.X > Ordinates'First
                  when East =>
                     if Current_SE.Coordinate.X < X_High then
                        Next_SE.Coordinate.X := Current_SE.Coordinate.X + 1;
                        Conditional_Enqueue (Next_SE, Search_Set);
                     end if; -- Current_SE.Coordinate.X > Ordinates'First
                  when West =>
                     if Current_SE.Coordinate.X > Ordinates'First then
                        Next_SE.Coordinate.X := Current_SE.Coordinate.X - 1;
                        Conditional_Enqueue (Next_SE, Search_Set);
                     end if; -- Current_SE.Coordinate.X > Ordinates'First
                  end case; -- Current_SE.Direction
            end case;
         else
            case Current_SE.Direction is
               when North =>
                  if Current_SE.Coordinate.Y > Ordinates'First then
                     Next_SE.Coordinate.Y := Current_SE.Coordinate.Y - 1;
                     Conditional_Enqueue (Next_SE, Search_Set);
                  end if; -- Current_SE.Coordinate.Y > Ordinates'First
               when South =>
                  if Current_SE.Coordinate.Y < Y_High then
                     Next_SE.Coordinate.Y := Current_SE.Coordinate.Y + 1;
                     Conditional_Enqueue (Next_SE, Search_Set);
                  end if; -- Current_SE.Coordinate.Y < Y_High
               when East =>
                  if Current_SE.Coordinate.X < X_High then
                     Next_SE.Coordinate.X := Current_SE.Coordinate.X + 1;
                     Conditional_Enqueue (Next_SE, Search_Set);
                  end if; -- Current_SE.Coordinate.X > Ordinates'First
               when West =>
                  if Current_SE.Coordinate.X > Ordinates'First then
                     Next_SE.Coordinate.X := Current_SE.Coordinate.X - 1;
                     Conditional_Enqueue (Next_SE, Search_Set);
                  end if; -- Current_SE.Coordinate.X > Ordinates'First
            end case; -- Current_SE.Direction
         end if; -- Contains (Optical_Store, Current_SE.Coordinate)
      end loop; -- Queue.Current_Use > 0
   end Ray_Trace;

   Optical_Store : Optical_Stores.Map;
   X_High, Y_High : Ordinates;
   Tile_Set : Tile_Sets.Set;
   Maximum : Count_Type := Count_Type'First;

begin -- December_16
   Read_input (Optical_Store, X_High, Y_High);
   Ray_Trace (Optical_Store, X_High, Y_High, 1, 1, East, Tile_Set);
   Put_Line ("Part one:" & Length (Tile_Set)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   for X in Ordinates range Ordinates'First .. X_High loop
      for D in Directions loop
         Ray_Trace (Optical_Store, X_High, Y_High, X, Ordinates'First, D,
                    Tile_Set);
         if Maximum < Length (Tile_Set) then
            Maximum := Length (Tile_Set);
         end if; -- Maximum < Length (Tile_Set)
         Ray_Trace (Optical_Store, X_High, Y_High, X, Y_High, D, Tile_Set);
         if Maximum < Length (Tile_Set) then
            Maximum := Length (Tile_Set);
         end if; -- Maximum < Length (Tile_Set)
      end loop; -- D in Directions
   end loop; -- X in Ordinates range Ordinates'First .. X_High
   for Y in Ordinates range Ordinates'First .. X_High loop
      for D in Directions loop
         Ray_Trace (Optical_Store, X_High, Y_High, Ordinates'First, Y, D,
                    Tile_Set);
         if Maximum < Length (Tile_Set) then
            Maximum := Length (Tile_Set);
         end if; -- Maximum < Length (Tile_Set)
         Ray_Trace (Optical_Store, X_High, Y_High, X_High, Y, D, Tile_Set);
         if Maximum < Length (Tile_Set) then
            Maximum := Length (Tile_Set);
         end if; -- Maximum < Length (Tile_Set)
      end loop; -- D in Directions
   end loop; -- Y in Ordinates range Ordinates'First .. X_High
   Put_Line ("Part two:" & Maximum'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_16;
