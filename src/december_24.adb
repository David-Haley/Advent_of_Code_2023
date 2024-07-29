with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Numerics.Big_Numbers.Big_Reals; use Ada.Numerics.Big_Numbers.Big_Reals;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_24 is

   subtype My_Float is Big_Real;

   subtype Ordinates is Big_Integer;

   type Vector_3 is record
      X, Y, Z : Ordinates;
   end record; -- Vector_3

   type Hail_Stones is record
      P, V : Vector_3;
   end record; -- Hail_Stones

   type Planes is record
      V : Vector_3; -- intersection with axies
      O : Ordinates; -- offset from origin
   end record; -- Planes

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
         Lower_Bound :=  From_String (Argument (2));
         Upper_Bound :=  From_String (Argument (3));
      else
         Lower_Bound := 200000000000000.0;
         Upper_Bound := 400000000000000.0;
      end if; -- Argument_Count = 3
      Clear (Hail_Stone_List);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := + 1;
         Find_Token (Text, Delimiters, Start_at, Outside, First, Last);
         Hail_Stone.P.X := From_String (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Delimiters, Start_at, Outside, First, Last);
         Hail_Stone.P.Y :=  From_String (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Delimiters, Start_at, Outside, First, Last);
         Hail_Stone.P.Z :=  From_String (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Delimiters, Start_at, Outside, First, Last);
         Hail_Stone.V.X :=  From_String (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Delimiters, Start_at, Outside, First, Last);
         Hail_Stone.V.Y :=  From_String (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Delimiters, Start_at, Outside, First, Last);
         Hail_Stone.V.Z :=  From_String (Slice (Text, First, Last));
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

      A1 : constant My_Float :=
        - To_Big_Real (Stone_1.V.Y) / To_Big_Real (Stone_1.V.X);
      C1 : constant My_Float := To_Big_Real (Stone_1.P.Y)
        - To_Big_Real (Stone_1.P.X * Stone_1.V.Y) / To_Big_Real (Stone_1.V.X);
      A2 : constant My_Float :=
        - To_Big_Real (Stone_2.V.Y) / To_Big_Real (Stone_2.V.X);
      C2 : constant My_Float := To_Big_Real (Stone_2.P.Y)
        - To_Big_Real (Stone_2.P.X * Stone_2.V.Y) / To_Big_Real (Stone_2.V.X);
      Denominator : constant My_Float := A1 - A2;
      T1, T2 : My_Float;

   begin -- Solve_For_X_Y
      No_Intersection := Denominator = 0.0;
      if No_Intersection then
         X := 0.0; -- not a result but valid
         Y := 0.0;
      else
         X := (C1 - C2) / Denominator;
         Y := (A1 * C2 - A2 * C1) / Denominator;
         T1 := (X - To_Big_Real (Stone_1.P.X)) / To_Big_Real (Stone_1.V.X);
         T2 := (X - To_Big_Real (Stone_2.P.X)) / To_Big_Real (Stone_2.V.X);
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

   Function Part_2 (Hail_Stone_List : Hail_Stone_Lists.List)
                    return Ordinates is

      -- Solution derived from Redit post below:
      -- https://www.reddit.com/r/adventofcode/comments/18pnycy/comment/kersplf/
      -- ?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_
      -- term=1&utm_content=share_button
      -- The czlculations below exceed 64 bit integer range so Big_Number
      -- support has been used. In Ada this is a bit clunky (added in 2022) and
      -- slow. Part 2 executes quickly only three hail stones are used as input
      -- But part 1 is slowed down dramatically from using machine reals.

      function Dot (A, B : Vector_3) return Ordinates is
        (A.X * B.X + A.Y * B.Y + A.Z * B.Z);

      function Cross (A, B : Vector_3) return Vector_3 is
        (A.Y * B.Z - A.Z * B.Y, A.Z * B.X - A.X * B.Z, A.X * B.Y - A.Y * B.X);

      function "-" (A, B : Vector_3) return Vector_3 is
        (A.X - B.X, A.Y - B.Y, A.Z - B.Z);

      function Independent (Stone_1, Stone_2 : in Hail_Stones) return Boolean is

         V1xV2 : Vector_3 := Cross (Stone_1.V, Stone_2.V);

      begin -- Independent
         return V1xV2.X /= 0 or V1xV2.Y /= 0 or V1xV2.Z /= 0;
      end Independent;

      function Find_Plane (Stone_1, Stone_2 : in Hail_Stones) return Planes is
        ((Cross (Stone_1.P - Stone_2.P, Stone_1.V - Stone_2.V),
         Dot (Stone_1.P - Stone_2.P, Cross (Stone_1.V, Stone_2.V))));

      function Find_Point (A, B, C : in Planes) return Vector_3 is

         Point : Vector_3;

      begin -- Find_Point
         Point.X := A.V.X * A.O + B.V.X * B.O + C.V.X * C.O;
         Point.Y := A.V.Y * A.O + B.V.Y * B.O + C.V.Y * C.O;
         Point.Z := A.V.Z * A.O + B.V.Z * B.O + C.V.Z * C.O;
         return Point;
      end Find_Point;

      Stone_1 : Hail_Stones := First_Element(Hail_Stone_List);
      Stone_2, Stone_3 : Hail_Stones;
      Sc : Hail_Stone_Lists.Cursor := Next (First (Hail_Stone_List));
      A, B, C : Planes;
      Rock, W, W1, W2, WW : Vector_3;
      Scale, T, E, F, G : Ordinates;

   begin -- Part_2
      while not Independent (Stone_1, Element (Sc)) loop
         Next (Sc);
      end loop; -- not Independent (Stone_1, Element (Sc)) loop
      Stone_2 := Element (Sc);
      Next (Sc);
      while not (Independent (Stone_1, Element (Sc)) and
                   Independent (Stone_2, Element (Sc))) loop
         Next (Sc);
      end loop; -- not (Independent (Stone_1, Element (Sc)) and ..
      Stone_3 := Element (Sc);
      A := Find_Plane (Stone_1, Stone_2);
      B := Find_Plane (Stone_1, Stone_3);
      C := Find_Plane (Stone_2, Stone_3);
      W := Find_Point ((Cross (B.V, C.V), A.O),
                       (Cross (C.V, A.V), B.O),
                       (Cross (A.V, B.V), C.O));
      T := Dot (A.V, Cross (B.V, C.V));
      W := (W.X / T, W.Y / T, W.Z / T);
      W1 := Stone_1.V - W;
      W2 := Stone_2.V - W;
      WW := Cross (W1, W2);
      E := Dot (WW, Cross (Stone_2.P, W2));
      F := Dot (WW, Cross (Stone_1.P, W1));
      G := Dot (Stone_1.P, WW);
      Scale := Dot (WW, WW);
      Rock := Find_Point ((W1, E), (W2, -F), (WW, G));
      return  (Rock.X + Rock.Y + Rock.Z) / Scale;
   end Part_2;

   Hail_Stone_List : Hail_Stone_Lists.List;
   Lower_Bound, Upper_Bound : My_Float;

begin -- December_24
   Read_input (Hail_Stone_List, Lower_Bound, Upper_Bound);
   Put_Line ("Part one:" & Count_Intersections (Hail_Stone_List, Lower_Bound,
             Upper_Bound)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" & Part_2 (Hail_Stone_List)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_24;
