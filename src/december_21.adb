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

procedure December_21 is

   subtype Ordinates is Natural;
   -- Actual coordinates start at 1 avoids additional testing when taking a
   -- step in a negative direction.

   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   type Directions is (North, South, East, West);

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.X < Right.X or (Left.X = Right.x and Left.Y < Right.Y));

   package Gardens is new
     Ada.Containers.Ordered_Maps (Coordinates, Boolean);
   use Gardens;

   procedure Read_Input (Garden : out Gardens.Map;
                         Start : out Coordinates) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Y : Ordinates;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_21.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Garden);
      while not End_Of_File (Input_File) loop
         Y := Ordinates (Line (Input_File));
         Get_Line (Input_File, Text);
         for X in Positive range 1 .. Length (Text) loop
            if Element (Text, X) = '.' then
               insert (Garden, (X, Y), False);
            elsif Element (Text, X) = 'S' then
               Start := (X, Y);
               insert (Garden, (X, Y), False);
               -- S also garden plot
            end if; -- Element (Text, X) = '.'
         end loop; -- X in Positive range 1 .. Length (Text)
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   function Visited_Plots (Garden : in out Gardens.Map;
                           Start : in Coordinates;
                           Max_Steps : in Positive) return Count_Type is

      subtype Steps is Natural range 0 .. Max_Steps;

      package Plot_Stores is new Ada.Containers.Ordered_Sets (Coordinates);
      use Plot_Stores;

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

      Queue : Search_Queue.Queue;

      procedure Conditional_Enqueue (Current_SE : in Search_Elements;
                                     Direction : in Directions;
                                     Garden : in out Gardens.Map) is

         function Valid (Search_Element : in Search_Elements;
                         Garden : in Gardens.Map)
                         return Boolean is
           (Contains (Garden, Search_Element.Coordinate)
            and then not (Garden (Search_Element.Coordinate) and
                Search_Element.Step mod 2 = Steps'Last mod 2));
         -- Is a garden plot and hasn't previously been reached with the
         -- even count.

         pragma Inline_Always (Valid);

         Next_SE : Search_Elements := Current_SE;

      begin -- Conditional_Enqueue
         Next_SE.Step := @ + 1;
         case Direction is
            when North =>
               Next_SE.Coordinate.Y := @ - 1;
            when East =>
               Next_SE.Coordinate.X := @ + 1;
            when South =>
               Next_SE.Coordinate.Y := @ + 1;
            when West =>
               Next_SE.Coordinate.X := @ - 1;
         end case; -- Direction
         if Valid (Next_SE, Garden) then
            Garden (Next_SE.Coordinate) :=
              Garden (Next_SE.Coordinate) or
              Next_SE.Step mod 2 = Steps'Last mod 2;
            Queue.Enqueue (Next_SE);
         end if; -- Valid (Next_SE, Garden
      end  Conditional_Enqueue;

      Current_SE : Search_Elements := (Start, 0);
      Plot_Store : Plot_Stores.Set := Plot_Stores.Empty_Set;

   begin -- Visited_Plots
      Queue.Enqueue (Current_SE);
      While Queue.Current_Use > 0 loop
         Queue.Dequeue (Current_SE);
         if Current_SE.Step = Steps'Last then
            include (Plot_Store, Current_SE.Coordinate);
         else
            Conditional_Enqueue (Current_SE, North, Garden);
            Conditional_Enqueue (Current_SE, East, Garden);
            Conditional_Enqueue (Current_SE, South, Garden);
            Conditional_Enqueue (Current_SE, West, Garden);
         end if; -- Current_SE.Step = Steps'Last
      end loop; -- Queue.Current_Use > 0
      for G in Iterate (Garden) loop
         if Garden (G) then
            include (Plot_Store, Key (G));
         end if; -- Garden (G)
      end loop; -- G in Iterate Garden)
      return Length (Plot_Store);
   end Visited_Plots;

   Max_Steps : Positive := 64;
   Garden : Gardens.Map;
   Start : Coordinates;

begin -- December_21
   if Argument_Count = 2 then
      Max_Steps := Positive'Value (Argument (2));
   end if; -- Argument_Count = 2
   Read_input (Garden, Start);
   Put_Line ("Part one:" & Visited_Plots (Garden, Start, Max_Steps)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_21;
