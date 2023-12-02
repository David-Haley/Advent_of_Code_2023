with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Doubly_Linked_Lists;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_02 is

   type Colours is (Red, Green, Blue);
   type Sub_Games is array (Colours) of Natural;

   package Sub_Game_Stores is new
     Ada.Containers.Doubly_Linked_Lists (Sub_Games);
   use Sub_Game_Stores;

   package Game_Stores is new
     Ada.Containers.Vectors (Positive, Sub_Game_Stores.List);
   use Game_Stores;

   procedure Read_input (Game_Store : out Game_Stores.Vector) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Game_ID : Positive;
      Sub_Game :Sub_Games;
      Colour : Colours;
      Cubes : Natural;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_02.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Game_Store);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Append (Game_Store, Sub_Game_Stores.Empty_List);
         Game_ID := Last_Index (Game_Store);
         Start_At := 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, inside, First, Last);
         if Game_ID /= Positive'Value (Slice (Text, First, Last)) then
            raise Program_Error with "Bad Game ID expected " &
            Game_ID'Img & " read " & Slice (Text, First, Last);
         end if; -- Game_ID /= Positive'Value (Sloce (Text, First, Last))
         Start_At := Last + 1;
         if Element (Text, Start_At) /= ':' then
            raise Program_Error with "In Game" & Game_ID'Img &
              " expected ':' and found '" & Element (Text, Start_At) & "'";
         end if; -- Element (Text, Start_At) /= ':'
         loop -- Read one Game
            Sub_Game := (0, 0, 0);
            loop -- Read one Sub_Game
               Find_Token (Text, Decimal_Digit_Set, Start_At, inside, First,
                           Last);
               Cubes := Natural'Value (Slice (Text, First, Last));
               Start_At := Last + 1;
               Find_Token (Text, Lower_Set, Start_At, inside, First, Last);
               Colour := Colours'Value (Slice (Text, First, Last));
               Sub_Game (Colour) := Cubes;
               Start_At := Last + 1;
               exit when Start_At > Length (Text) or else
                 Element (Text, Start_At) = ';';
            end loop; -- Read one Sub_Game
            Append (Game_Store (Game_ID), Sub_Game);
            exit when Start_At > Length (Text);
         end loop; -- Read one Game
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   function Possible (Sub_Game_Store : in Sub_Game_Stores.List)
                      return Boolean is

      Result : Boolean := True;

   begin -- Possible
      for S in iterate (Sub_Game_Store) loop
         Result := Result and
           Element (S) (Red) <= 12 and
           Element (S) (Green) <= 13 and
           Element (S) (Blue) <= 14;
      end Loop; -- S in iterate (Sub_Game_Store)
      return Result;
   end Possible;

   function Power (Sub_Game_Store : in Sub_Game_Stores.List)
                   return Natural is

      Max_Cubes : Sub_Games := (0, 0, 0);

   begin -- Power
      for S in iterate (Sub_Game_Store) loop
         for C in Colours loop
            if Element (S) (C) > Max_Cubes (C) then
               Max_Cubes (C) := Element (S) (C);
            end if; -- Element (S) (C) > Max_Cubes (C)
         end loop; -- C in Colours
      end Loop; -- S in iterate (Sub_Game_Store)
      return Max_Cubes (Red) * Max_Cubes (Green) * Max_Cubes (Blue);
   end Power;

   Game_Store : Game_Stores.Vector;
   Sum : Natural;

begin -- December_02
   Read_input (Game_Store);
   Sum := 0;
   for G in Iterate (Game_Store) loop
      if Possible (Element (G)) then
         Sum := Sum + To_Index (G);
      end if; -- Possible (Element (G))
   end loop; -- G in Iterate (Game_Store)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Sum := 0;
   for G in Iterate (Game_Store) loop
      Sum := Sum + Power (Element (G));
   end loop; -- G in Iterate (Game_Store)
   Put_Line ("Part two:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_02;
