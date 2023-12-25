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

procedure December_24 is

   type My_Float is digits 15;

   type Hail_Stones is record
      Px, Py, Pz : My_Float;
      Vx, Vy, Vz : My_Float;
   end record; -- Hail_Stones

   type Lines is record
      X1, Y1, X2, Y2 : My_Float;
   end record; -- Lines

   package Hail_Stone_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Hail_Stones);
   use Hail_Stone_Lists;

   procedure Read_input (Hail_Stone_List : out Hail_Stone_Lists.List;
                         Lower_Bound, Upper_Bound : out My_Float) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : natural;
      Delimiters : constant Character_Set := To_Set (" ,@");
      Hail_Stone : Hail_Stones;


   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_24.txt");
      else
         Open (Input_File, In_File, Argument(1));
         if Argument_Count = 3 then
            Lower_Bound := My_Float'Value (Argument (2));
            Upper_Bound := My_Float'Value (Argument (3));
         else
            Lower_Bound := 200000000000000.0;
            Upper_Bound := 400000000000000.0;
         end if; -- Argument_Count = 3
      end if; -- Argument_Count = 0
      Clear (Hail_Stone_List);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Put_Line (Text);
         Start_At := + 1;
         Find_Token (Text, Delimiters, Start_at, Outside, First, Last);
         Hail_Stone.Px := My_Float'Value (Slice (Text, First, Last) & ".0");
         Start_At := Last + 1;
         Find_Token (Text, Delimiters, Start_at, Outside, First, Last);
         Hail_Stone.Py := My_Float'Value (Slice (Text, First, Last) & ".0");
         Start_At := Last + 1;
         Find_Token (Text, Delimiters, Start_at, Outside, First, Last);
         Hail_Stone.Pz := My_Float'Value (Slice (Text, First, Last) & ".0");
         Start_At := Last + 1;
         Find_Token (Text, Delimiters, Start_at, Outside, First, Last);
         Hail_Stone.Vx := My_Float'Value (Slice (Text, First, Last) & ".0");
         Start_At := Last + 1;
         Find_Token (Text, Delimiters, Start_at, Outside, First, Last);
         Hail_Stone.Vy := My_Float'Value (Slice (Text, First, Last) & ".0");
         Start_At := Last + 1;
         Find_Token (Text, Delimiters, Start_at, Outside, First, Last);
         Hail_Stone.Vz := My_Float'Value (Slice (Text, First, Last) & ".0");
         Start_At := Last + 1;
         Append (Hail_Stone_List, Hail_Stone);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   procedure In_Box_Line (Hail_Stone : in Hail_Stones;
                          Lower_Bound, Upper_Bound : in My_Float;
                          Line : out Lines;
                          Valid : out Boolean) is

      Txl, Txu, Tyl, Tyu : My_Float;
      -- times when the hail stone crosses the X and Y bounds, negative times
      -- can be ignored. This occurs if the hail stone in inside the box at
      -- t = 0. Further checks are required to find the if the actual crossings.

   begin -- In_Box_Line
      Valid := False;
      Line := (0.0, 0.0, 0.0, 0.0);
      Txl := (Lower_Bound - Hail_Stone.Px) / Hail_Stone.Vx;
      Txu := (Upper_Bound - Hail_Stone.Px) / Hail_Stone.Vx;
      Tyl := (Lower_Bound - Hail_Stone.Py) / Hail_Stone.Vy;
      Tyu := (Upper_Bound - Hail_Stone.Py) / Hail_Stone.Vy;
      Put_Line (Txl'Img & Txu'Img & Tyl'Img & Tyu'Img);
      Put_Line ("(" & My_Float'Image (Hail_Stone.Px + Hail_Stone.Vx * Txl) & "," & My_Float'Image (Hail_Stone.Py + Hail_Stone.Vy * Txl) & ")");
      Put_Line ("(" & My_Float'Image (Hail_Stone.Px + Hail_Stone.Vx * Txu) & "," & My_Float'Image (Hail_Stone.Py + Hail_Stone.Vy * Txu) & ")");
      Put_Line ("(" & My_Float'Image (Hail_Stone.Px + Hail_Stone.Vx * Tyl) & "," & My_Float'Image (Hail_Stone.Py + Hail_Stone.Vy * Tyl) & ")");
      Put_Line ("(" & My_Float'Image (Hail_Stone.Px + Hail_Stone.Vx * Tyu) & "," & My_Float'Image (Hail_Stone.Py + Hail_Stone.Vy * Tyu) & ")");
   end In_Box_Line;

   Hail_Stone_List : Hail_Stone_Lists.List;
   Lower_Bound, Upper_Bound : My_Float;
   Line : Lines;
   Valid : Boolean;

begin -- December_24
   Read_input (Hail_Stone_List, Lower_Bound, Upper_Bound);
   Put_Line (Hail_Stone_List'Img);
   for H in Iterate (Hail_Stone_List) loop
      In_Box_Line (Element (H), Lower_Bound, Upper_Bound, Line, Valid);
   end loop; -- H in Iterate (Hail_Stone_List)
   Put_Line ("Part one:");
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_24;
