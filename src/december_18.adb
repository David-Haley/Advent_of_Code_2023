with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Interfaces; use Interfaces;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_18 is

   subtype Ordinates is integer;
   -- allows 0 to be off map

   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   type Directions is (Up, Down, Right, Left);

   type States is record
      Coordinate : Coordinates;
      Direction : Directions;
   end record; -- States

   type Trench_Elements is array (Directions) of Boolean;
   -- True indicates entrance or exit possible

   function "<" (Left, Right : Coordinates) return Boolean is
      (Left.X < Right.X or (Left.X = Right.x and Left.Y < Right.Y));

   package Trenchs is new
     Ada.Containers.Ordered_Maps (Coordinates, Trench_Elements);
   use Trenchs;

   subtype Metres is Positive;
   subtype Colour_Values is Unsigned_32;

   type Dig_Plan_Elements is record
      Direction : Directions;
      Meter : Metres;
      Colour : Colour_Values;
   end record; --Dig_Plan_Elements

   package Dig_Plans is new
     Ada.Containers.Doubly_Linked_Lists (Dig_Plan_Elements);
   use Dig_Plans;

   procedure Read_input (Dig_Plan : out Dig_Plans.List) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : natural;
      Dig_Plan_Element : Dig_Plan_Elements;


   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_18.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Dig_Plan);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         case Element (Text, Start_At) is
            when 'U' =>
               Dig_Plan_Element.Direction := Up;
            when 'D' =>
               Dig_Plan_Element.Direction := Down;
            when 'L' =>
               Dig_Plan_Element.Direction := Left;
            when 'R' =>
               Dig_Plan_Element.Direction := Right;
            when others =>
               raise Program_Error with "illegal character '" &
                 Element (Text, Start_At) & "'";
         end case; -- Element (Text, X)
         Start_At := @ + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_at, Inside, First, Last);
         Dig_Plan_Element.Meter := Metres'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Hexadecimal_Digit_Set, Start_at, Inside, First,
                     Last);
         Dig_Plan_Element.Colour :=
           Colour_Values'Value ("16#" & Slice (Text, First, Last) & '#');
         First := @ + 2;
         Append (Dig_Plan, Dig_Plan_Element);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   procedure Dig_Trench (Trench : out Trenchs.Map;
                         Dig_Plan : in Dig_Plans.List) is

      Trench_Element : Trench_Elements := (others => False);
      Coordinate, Previous : Coordinates := (0, 0);

   begin -- Dig_Trench
      Clear (Trench);
      Insert (Trench, Coordinate, Trench_Element);
      for D in Iterate (Dig_Plan) loop
         Trench (Previous) (Element (D).Direction) := True;
         Trench_Element := (others => False);
         Trench_Element (Element (D).Direction) := True;
         for M in Metres range 1 .. Element (D).Meter loop
            case Element (D).Direction is
               when Up =>
                  Coordinate.Y := @ - 1;
               when Down =>
                  Coordinate.Y := @ + 1;
               when Left =>
                  Coordinate.X := @ - 1;
               when Right =>
                  Coordinate.X := @ + 1;
            end case; -- Element (D).Direction
            if Coordinate = (0, 0) then
               Trench (First (Trench)) (Element (D).Direction) := True;
            else
               insert (Trench, Coordinate, Trench_Element);
            end if; -- Coordinate = (0, 0)
         end loop; -- M in Metres range 1 .. Element (D).Meter
         Previous := Coordinate;
      end loop; -- D in Iterate (Dig_Plan)
   end Dig_Trench;

   procedure Dig_Trench_2 (Trench : out Trenchs.Map;
                         Dig_Plan : in Dig_Plans.List) is

      Trench_Element : Trench_Elements := (others => False);
      Coordinate, Previous : Coordinates := (0, 0);
      Metre : Metres;
      Direction : Directions;

   begin -- Dig_Trench_2
      Clear (Trench);
      Insert (Trench, Coordinate, Trench_Element);
      for D in Iterate (Dig_Plan) loop
         case Element (D).Colour and 2#11# is
            when 0 => Direction := Right;
            when 1 => Direction := Down;
            when 2 => Direction := Left;
            when 3 => Direction := Up;
            when others => raise Program_Error with
                 "HELP can't possibly get here";
         end case; -- Element (D).Colour and 16#000003#
         Metre := Metres (Shift_Right (Element (D).Colour, 4));
         Trench (Previous) (Direction) := True;
         Trench_Element := (others => False);
         Trench_Element (Direction) := True;
         for M in Metres range 1 .. Metre loop
            case Direction is
               when Up =>
                  Coordinate.Y := @ - 1;
               when Down =>
                  Coordinate.Y := @ + 1;
               when Left =>
                  Coordinate.X := @ - 1;
               when Right =>
                  Coordinate.X := @ + 1;
            end case; -- Direction
            if Coordinate = (0, 0) then
               Trench (First (Trench)) (Direction) := True;
            else
               insert (Trench, Coordinate, Trench_Element);
            end if; -- Coordinate = (0, 0)
         end loop; -- M in Metres range 1 .. Meter
         Previous := Coordinate;
      end loop; -- D in Iterate (Dig_Plan)
   end Dig_Trench_2;

   function Inside_Count (Trench_Map : in Trenchs.Map) return Natural is

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
      for L in Iterate (Trench_Map) loop
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
      end loop; -- L in Iterate (Trench_Map)
      for Y in Ordinates range Y_Low .. Y_High loop
         Edge_Count := 0;
         Previous_NS := Right; -- has to be anything other than Up or Down
         In_Loop := False;
         for X in Ordinates range X_Low .. X_High loop
            if Contains (Trench_Map, (X, Y)) then
               -- Don't count twice for entrances and exits from a row in the
               -- same direction
               if Trench_Map ((X, Y)) (Up) and
                 not (Previous_NS = Up and In_Loop) then
                  Edge_Count := Edge_Count + 1;
               elsif Trench_Map ((X, Y)) (Down) and
                 not (Previous_NS = Down and In_Loop) then
                  Edge_Count := Edge_Count - 1;
               end if; -- Trench ((X, Y)) (Up)
               -- Set In_loop False if the row has been exited in the same
               -- direction as it was entered
               if In_Loop and then
                 ((Trench_Map ((X, Y)) (Up) and Previous_NS = Up) or
                    (Trench_Map ((X, Y)) (Down) and Previous_NS = Down)) then
                  In_Loop := False;
                  Previous_NS := Right;
                  -- Anything other than Up or Down but it may not be
                  -- essential to make it invalid here;
               end if; -- In_Loop and then
               -- In_Loop becomes true when a bend is in the current row and
                       -- remains true whilst there is a connection in that row.
               In_Loop := (In_loop or Trench_Map ((X, Y)) (Up) or
                             Trench_Map ((X, Y)) (Down)) and
                 (Trench_Map ((X, Y)) (Right) or Trench_Map ((X, Y)) (Left));
               if Trench_Map ((X, Y)) (Up) then
                  Previous_NS := Up;
               elsif Trench_Map ((X, Y)) (Down) then
                  Previous_NS := Down;
               end if; -- Trench_Map ((X, Y)) (Up)
            end if; -- Contains (Trench_Map, (X, Y))
            if Edge_Count mod 2 = 1 and not Contains (Trench_Map, (X, Y)) then
               Count := Count + 1;
            end if; -- Edge_Count mod 2 = 1 and not Contains (Trench_Map, ...
         end loop; -- X in Ordinates range X_Low .. X_High
      end loop; -- Y in Ordinates range Y_Low .. Y_High
      return Count;
   end Inside_Count;

   Dig_Plan : Dig_Plans.List;
   Trench : Trenchs.Map;

begin -- December_18
   Read_input (Dig_Plan);
   Dig_Trench (Trench, Dig_Plan);
   Put_Line ("Part one:" & Positive'Image (Positive (Length (Trench)) +
               Inside_Count (Trench)));
   DJH.Execution_Time.Put_CPU_Time;
   Dig_Trench_2 (Trench, Dig_Plan);
   Put_Line ("Part two:" & Positive'Image (Positive (Length (Trench)) +
               Inside_Count (Trench)));
   DJH.Execution_Time.Put_CPU_Time;
end December_18;
