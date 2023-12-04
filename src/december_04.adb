with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_04 is

   subtype Card_Numbers is Positive range 1 .. 99;
   subtype Card_Indices is Positive;

   package Card_Sets is new Ada.Containers.Ordered_Sets (Card_Numbers);
   use Card_Sets;

   type Cards is record
      Winning_Set, My_Set : Card_Sets.Set := Card_Sets.Empty_Set;
   end record; -- Cards

   package Card_Stores is new
     Ada.Containers.Vectors (Card_Indices, Cards);
   use Card_Stores;

   procedure Read_input (Card_Store : out Card_Stores.Vector) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last, Solidus : Natural;
      Card_ID : Positive;
      Card : constant Cards := (Card_Sets.Empty_Set, Card_Sets.Empty_Set);

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_04.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Card_Store);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Append (Card_Store, Card);
         Card_ID := Last_Index (Card_Store);
         Start_At := 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, inside, First, Last);
         if Card_ID /= Card_Indices'Value (Slice (Text, First, Last)) then
            raise Program_Error with "Bad Card ID expected " &
              Card_ID'Img & " read " & Slice (Text, First, Last);
         end if; -- Card_ID /= Card_Indices'Value (Sloce (Text, First, Last))
         Start_At := Last + 1;
         if Element (Text, Start_At) /= ':' then
            raise Program_Error with "In Card" & Card_ID'Img &
              " expected ':' and found '" & Element (Text, Start_At) & "'";
         end if; -- Element (Text, Start_At) /= ':'
         Solidus := Index (Text, "|",  Start_At);
         if Solidus = 0 then
            raise Program_Error with "In Card" & Card_ID'Img &
              "Solidus not Found";
         end if; -- Solidus = 0
         loop -- Read one Number
            Find_Token (Text, Decimal_Digit_Set, Start_At, inside, First,
                        Last);
            exit when Last = 0; -- allow for white space after last number
            Start_At := Last + 1;
            if First < Solidus then
               Insert (Card_Store (Card_ID).Winning_Set,
                       Card_Numbers'Value (Slice (Text, First, Last)));
            else
               Insert (Card_Store (Card_ID).My_Set,
                       Card_Numbers'Value (Slice (Text, First, Last)));
            end if; -- First < Solidus
            exit when Start_At > Length (Text);
         end loop; -- Read one Number
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   function Winners (Card : in Cards) return Natural is
     (Natural (Length (Intersection (Card.Winning_Set, Card.My_Set))));

   function Points (Card : in Cards) return Natural is

   begin -- Points
      if Winners (Card) > 0 then
         return 2 ** (Winners (Card) - 1);
      else
         return 0;
      end if; -- Winners > 0
   end Points;

   function Scratch_Cards (Card_Store : in Card_Stores.Vector) return Natural is

      subtype Card_Id is Positive range 1 .. Last_Index (Card_Store);
      Card_Count : array (Card_Id) of Positive := (others => 1);

      Sum : Natural := 0;

   begin --Scratch_Cards
      for C in Iterate (Card_Store) loop
         for I in Card_Indices range To_Index (C) + 1 ..
           Winners (Element (C)) + To_Index (C) loop
            Card_Count (I) := Card_Count (I) +
              Card_Count (To_Index (C));
         end loop; -- I in Card_Indices range To_Index (C) + 1 ...
      end loop; -- C in Iterate (Card_Store)
      for I in Card_Id loop
         Sum := Sum + Card_Count (I);
      end loop; -- I in Card_Id
      return Sum;
   end Scratch_Cards;

   Card_Store : Card_Stores.Vector;
   Sum : Natural;

begin -- December_04
   Read_input (Card_Store);
   Sum := 0;
   for C in Iterate (Card_Store) loop
      Sum := Sum + Points (Element (C));
   end loop; -- C in Iterate (Card_Store)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" & Scratch_Cards (Card_Store)'img);
   DJH.Execution_Time.Put_CPU_Time;
end December_04;
