with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;
with Interfaces; use Interfaces;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_18_Shoelace is

   -- This is an alternative answer for Day 18. it uses the Shoelace formular to
   -- calculate the area inside the trench. The end result is not particularly
   -- intuitive in that half the perimeter is added plus 1 (starting hole?).

   subtype Long_Natural is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   subtype Ordinates is Long_Long_Integer;

   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   type Directions is (Up, Down, Right, Left);

   subtype Metres is Long_Natural;
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

   function Area (Dig_Plan : in Dig_Plans.List;
                  Part_2 : Boolean := False) return Long_Natural is

      Coordinate, Previous : Coordinates := (0, 0);
      Metre : Metres;
      Direction : Directions;
      Result, Perimeter : Long_Long_Integer := 0;

   begin -- Area
      for D in Iterate (Dig_Plan) loop
         if Part_2 then
            case Element (D).Colour and 2#11# is
            when 0 => Direction := Right;
            when 1 => Direction := Down;
            when 2 => Direction := Left;
            when 3 => Direction := Up;
            when others => raise Program_Error with
                 "HELP can't possibly get here";
            end case; -- Element (D).Colour and 16#000003#
            Metre := Metres (Shift_Right (Element (D).Colour, 4));
         else
            Direction := Element (D).Direction;
            Metre := Element (D).Meter;
         end if;
         Perimeter := @ + Metre;
         case Direction is
            when Up =>
               Coordinate.Y := @ - Metre;
            when Down =>
               Coordinate.Y := @ + Metre;
            when Left =>
               Coordinate.X := @ - Metre;
            when Right =>
               Coordinate.X := @ + Metre;
         end case; -- Direction
         Result := @ + Previous.X * Coordinate.Y - Previous.Y * Coordinate.X;
         Previous := Coordinate;
      end loop; -- D in Iterate (Dig_Plan)
      return (Result + Perimeter) / 2 + 1;
   end Area;

   Dig_Plan : Dig_Plans.List;

begin -- December_18_Shoelace
   Read_input (Dig_Plan);
   Put_Line ("Part one:" & Area (Dig_Plan)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" &  Area (Dig_Plan, True)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_18_Shoelace;
