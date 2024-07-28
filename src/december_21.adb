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

   subtype Ordinates is Integer;

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

      -- Map is assumed to be square to return a valid Map_Size

      Input_File : File_Type;
      Text : Unbounded_String;
      Y : Ordinates;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_21.txt");
      else
         Open (Input_File, In_File, Argument (1));
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

      -- This has the side effect of setting true all the elements that can be
      -- visited in Max_Steps.

      subtype Steps is Natural range 0 .. Max_Steps;

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
            and then not Garden (Search_Element.Coordinate));
         -- Is a garden plot and hasn't previously been reached.

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
      Visited : Count_Type := 0;

   begin -- Visited_Plots
      Queue.Enqueue (Current_SE);
      While Queue.Current_Use > 0 loop
         Queue.Dequeue (Current_SE);
         if Current_SE.Step < Steps'Last then
            Conditional_Enqueue (Current_SE, North, Garden);
            Conditional_Enqueue (Current_SE, East, Garden);
            Conditional_Enqueue (Current_SE, South, Garden);
            Conditional_Enqueue (Current_SE, West, Garden);
         end if; -- Current_SE.Step < Steps'Last
      end loop; -- Queue.Current_Use > 0
      for G in iterate (Garden) loop
         if Garden (G) then
            Visited := @ + 1;
         end if; -- Garden (G)
      end loop; -- G in iterate (Garden)
      return Visited;
   end Visited_Plots;

   function Extrapolate (Garden : in Gardens.Map;
                         Start : in Coordinates) return long_Long_Integer is

      -- Calculation is based on the assumption that the number of plots
      -- visited is of the form Y(n) := A n ** 2 + B * n + C where n is the
      -- multiplier of the map size. This is not valid for the example!

      procedure Big_Map (Garden : in Gardens.Map;
                         N : in Positive;
                         Map_Size : in Ordinates;
                         Big_Garden : out Gardens.Map) is

      begin -- Big_Map
         clear (Big_Garden);
         for G in iterate (Garden) loop
            for X in Integer range -N .. N loop
               for Y in Integer range -N .. N loop
                  Insert (Big_Garden,
                          (Key (G).X + X * Map_Size,
                           Key (G).Y + Y * Map_Size),
                          False);
               end loop; -- Y in Integer range -N .. N
            end loop; -- X in Integer range -N .. N
         end loop; -- G in iterate (Garden)
      end Big_Map;

      Max_Steps_Part_2 : constant Positive := 26501365;
      Map_Size : constant Ordinates := 131;
      Initial_Steps : constant Positive := 65;
      Step_Multiplier : constant Long_Long_Integer :=
        Long_Long_Integer ((Max_Steps_Part_2 - Initial_Steps) / Map_Size);
      Big_Garden : Gardens.Map;
      A, B, C, Y1, Y2 : Long_Long_Integer;

   begin -- Extrapolate
      Big_Garden := Garden;
      -- Big_Garden is just a copy of Garden for n = 0.
      C := Long_Long_Integer (Visited_Plots (Big_Garden, Start, Initial_Steps));
      Big_Map (Garden, 1, Map_Size, Big_Garden);
      Y1 := Long_Long_Integer (Visited_Plots (Big_Garden, Start,
                               Map_Size + Initial_Steps));
      Big_Map (Garden, 2, Map_Size, Big_Garden);
      Y2 := Long_Long_Integer (Visited_Plots (Big_Garden, Start,
                               2 * Map_Size + Initial_Steps));
      B := (-Y2 + 4 * Y1 - 3 * C) / 2;
      A := Y1 -B -C;
      return A * Step_Multiplier ** 2 + B * Step_Multiplier + C;
   end Extrapolate;

   Max_Steps_Part_1 : Positive := 64;
   Garden : Gardens.Map;
   Start : Coordinates;

begin -- December_21
   if Argument_Count = 2 then
      Max_Steps_Part_1 := Positive'Value (Argument (2));
   end if; -- Argument_Count = 2 or Argument_Count = 3
   Read_input (Garden, Start);
   Put_Line ("Part one:" & Visited_Plots (Garden, Start, Max_Steps_Part_1)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Read_Input (Garden, Start);
   if Argument_Count < 2 then
      Put_Line ("Part two:" & Extrapolate (Garden, Start)'Img);
      DJH.Execution_Time.Put_CPU_Time;
   else
      Put_Line ("Part 2 solution only valid for specially constructed garden!");
   end if; -- Argument_Count < 2
end December_21;
