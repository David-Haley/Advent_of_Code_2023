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
      end if; -- Argument_Count = 0
      if Argument_Count = 3 then
         Lower_Bound := My_Float'Value (Argument (2));
         Upper_Bound := My_Float'Value (Argument (3));
      else
         Lower_Bound := 200000000000000.0;
         Upper_Bound := 400000000000000.0;
      end if; -- Argument_Count = 3
      Clear (Hail_Stone_List);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
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

   procedure Solve_For_X_Y (Stone_1, Stone_2 : in Hail_Stones;
                           No_Intersection : out Boolean;
                           X, Y : out My_Float) is

      -- Lines in the form Ax + By = C
      -- equations rearranged such that B = 1
      A1 : constant My_Float := - Stone_1.Vy / Stone_1.Vx;
      C1 : constant My_Float :=
        Stone_1.Py - Stone_1.Px * Stone_1.Vy/ Stone_1.Vx;
      A2 : constant My_Float := - Stone_2.Vy / Stone_2.Vx;
      C2 : constant My_Float :=
        Stone_2.Py - Stone_2.Px * Stone_2.Vy / Stone_2.Vx;
      Denominator : constant My_Float := A1 - A2;
      T1, T2 : My_Float;

   begin -- Solve_For_X_Y
      No_Intersection := Denominator = 0.0;
      if No_Intersection then
         X := 0.0; -- not a result but a valid real
         Y := 0.0;
      else
         X := (C1 - C2) / Denominator;
         Y := (A1 * C2 - A2 * C1) / Denominator;
         T1 := (X - Stone_1.Px) / Stone_1.Vx;
         T2 := (X - Stone_2.Px) / Stone_2.Vx;
         No_Intersection := T1 < 0.0 or T2 < 0.0;
         -- Intersection was prior to t = 0.0
      end if; -- No_Intersection
   end Solve_For_X_Y;

   function Count_Intersections (Hail_Stone_List : in Hail_Stone_Lists.List;
                                 Lower_Bound, Upper_Bound : in My_Float)
                                 return Natural is

      No_intersection : Boolean;
      S2 : Hail_Stone_Lists.Cursor;
      X, Y : My_Float;
      Count : Natural := 0;

   begin -- Count_Intersections
      for S1 in Iterate (Hail_Stone_List) loop
         S2 := Next (S1);
         while S2 /= Hail_Stone_Lists.No_Element loop
            Solve_For_X_Y (Element (S1), Element (S2), No_intersection, X, Y);
            if not No_intersection and then
              (Lower_Bound <= X and X <= Upper_Bound and
                 Lower_Bound <= Y and Y <= Upper_Bound) then
               Count := @ + 1;
            end if; -- not No_intersection and then ...
            Next (S2);
         end loop; -- S2 in Iterate (Hail_Stone_List, Next (S1)
      end loop; -- S1 in Iterate (Hail_Stone_List)
      return Count;
   end Count_Intersections;

   Hail_Stone_List : Hail_Stone_Lists.List;
   Lower_Bound, Upper_Bound : My_Float;

begin -- December_24
   Read_input (Hail_Stone_List, Lower_Bound, Upper_Bound);
   Put_Line ("Part one:" & Count_Intersections (Hail_Stone_List, Lower_Bound,
             Upper_Bound)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_24;
