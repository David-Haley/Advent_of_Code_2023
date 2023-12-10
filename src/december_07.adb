with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_07 is

   type Hand_Types is (High_card, One_pair, Two_pair, Three_of_a_kind,
                       Full_house, Four_of_a_kind, Five_of_a_kind);

   Part_1_Mapping : constant Character_Mapping :=To_Mapping ("AKQJT98765432",
                                                             "mlkjihgfedcba");

   Part_2_Mapping : constant Character_Mapping :=To_Mapping ("AKQT98765432J",
                                                             "mlkjihgfedcba");
   Wild_Card : constant Character := 'a';

   subtype Value_Indices is Character range 'a' .. 'm';

   type Hystograms is array (Value_Indices) of Natural;

   subtype Card_Indices is Positive range 1 .. 5;
   subtype Hands is String (Card_Indices);

   type Player_States is record
      Hand, Part_2, Modified_Hand : Hands;
      Bid : Positive;
      Hand_Type : Hand_Types := High_card;
   end record; -- Player_States

   package Game_Stores is new Ada.Containers.Vectors (Positive, Player_States);
   use Game_Stores;

   function Part_1_Less_Than (Left, Right : Player_States) return Boolean is

      Result, Decided : Boolean;
      C : Card_Indices := Card_Indices'First;

   begin -- Part_1_Less_Than
      if Left.Hand_Type = Right.Hand_Type then
         loop -- check one card
            Decided := Left.Hand (C) /= Right.Hand (C);
            Result := Left.Hand (C) < Right.Hand (C);
            exit when Decided or C = Card_Indices'Last;
            C := C + 1;
         end loop; -- check one card
      else
         Result := Left.Hand_Type < Right.Hand_Type;
      end if; -- Left.Hand_Type = Right.Hand_Type
      return Result;
   end Part_1_Less_Than;

   package Part_1_Sort is new
     Game_Stores.Generic_Sorting ("<" => Part_1_Less_Than);

   function Part_2_Less_Than (Left, Right : Player_States) return Boolean is

      Result, Decided : Boolean;
      C : Card_Indices := Card_Indices'First;

   begin -- Part_2_Less_Than
      if Left.Hand_Type = Right.Hand_Type then
         loop -- check one card
            Decided := Left.Part_2 (C) /= Right.Part_2 (C);
            Result := Left.Part_2 (C) < Right.Part_2 (C);
            exit when Decided or C = Card_Indices'Last;
            C := C + 1;
         end loop; -- check one card
      else
         Result := Left.Hand_Type < Right.Hand_Type;
      end if; -- Left.Hand_Type = Right.Hand_Type
      return Result;
   end Part_2_Less_Than;

   package Part_2_Sort is new
     Game_Stores.Generic_Sorting ("<" => Part_2_Less_Than);

   procedure Read_Input (Game_Store : out Game_Stores.Vector) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Player_State : Player_States;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_07.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Game_Store);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := Card_Indices'Last + 1;
         Player_State.Hand :=
           To_String (Translate (Unbounded_Slice (Text, Card_Indices'First,
                      Card_Indices'Last), Part_1_Mapping));
         Player_State.Part_2 :=
           To_String (Translate (Unbounded_Slice (Text, Card_Indices'First,
                      Card_Indices'Last), Part_2_Mapping));
         Player_State.Modified_Hand := Player_State.Part_2;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Player_State.Bid := Positive'Value (Slice (Text, First, Last));
         Append (Game_Store, Player_State);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   procedure Build (Hand : in Hands; Hystogram : out Hystograms) is

   begin -- Build
      Hystogram := (others => 0);
      for C in Card_Indices loop
         Hystogram (Hand (C)) :=
           Hystogram (Hand (C)) + 1;
      end loop; -- C in Card_Indices
   end Build;

   procedure Characterise (Game_Store : in out Game_Stores.Vector) is

      subtype Pair_Counts is Natural range 0 .. 2;

      function Pair_Count (Hystogram : in Hystograms) return Pair_Counts is

         Result : Pair_Counts := 0;

      begin -- Pair_Count
         for V in Value_Indices loop
            if Hystogram (V) = 2 then
               Result := Result + 1;
            end if; -- Hystogram (V) = 2
         end loop; -- V in Value_Indices
         Return Result;
      end Pair_Count;

      function Max_Count (Hystogram : in Hystograms) return Positive is

         Result : Positive := 1;

      begin -- Max_Count
         for V in Value_Indices loop
            if Hystogram (V) > Result then
               Result :=  Hystogram (V);
            end if; -- Hystogram (V) > Result
         end loop; -- V in Value_Indices
         return Result;
      end Max_Count;

      Hystogram : Hystograms;

   begin -- Characterise
      for G in Iterate (Game_Store) loop
         Build (Game_Store (G).Hand, Hystogram);
         if Max_Count (Hystogram) = 5 then
            Game_Store (G).Hand_Type := Five_of_a_kind;
         elsif Max_Count (Hystogram) = 4 then
            Game_Store (G).Hand_Type := Four_of_a_kind;
         elsif Max_Count (Hystogram) = 3 then
            if Pair_Count (Hystogram) = 1 then
               Game_Store (G).Hand_Type := Full_house;
            else
               Game_Store (G).Hand_Type := Three_of_a_kind;
            end if; -- Pair_Count (Hystogram) = 1
         elsif Pair_Count (Hystogram) = 2 then
            Game_Store (G).Hand_Type := Two_pair;
         elsif Pair_Count (Hystogram) = 1 then
            Game_Store (G).Hand_Type := One_pair;
         else
            Game_Store (G).Hand_Type := High_card;
         end if; -- Max_Count (Hystogram) = 5
      end loop; -- G in Iterate (Game_Store)
   end Characterise;

   procedure Upgrade_Hand (Game_Store : in out Game_Stores.Vector) is

      subtype Card_Counts is Positive range 1 .. 4;

      function Card_of_Count (Hystogram : in Hystograms;
                              Card_Count : in Card_Counts)
                              return Value_Indices is

         V : Value_Indices := Value_Indices'First;

      begin -- Card_of_Count
         while Hystogram (V) /= Card_Count loop
            V := Value_Indices'Succ (V);
            -- will raise exception if no card with Card_Count exixts
         end loop;
         return V;
      end Card_of_Count;

      Hystogram : Hystograms;
      Wild_Index, Not_Wild_Index : Card_Indices;
      Wild_Found, Not_Wild_Found : Boolean;

   begin -- Upgrade_Hand
      for G in Iterate (Game_Store) loop
         Build (Game_Store (G).Modified_Hand, Hystogram);
         while Hystogram (Wild_Card) > 0 and
           not (Game_Store (G).Hand_Type = Five_of_a_kind) loop
            Wild_Found := False;
            Not_Wild_Found := False;
            for C in Card_Indices loop
               if not Wild_Found and Game_Store (G).Modified_Hand (C) =
                 Wild_Card then
                  Wild_Index := C;
                  Wild_Found := True;
               end if; -- not Wild_Found and Game_Store (G).Modified_Hand ...
               if not Not_Wild_Found and Game_Store (G).Modified_Hand (C) /=
                 Wild_Card then
                  Not_Wild_Index := C;
                  Not_Wild_Found := True;
               end if; -- not Not_Wild_Found and Game_Store (G) ...
            end loop; -- C in Card_Indices
            case Game_Store (G).Hand_Type is
               when Five_of_a_kind =>
                  Null;
               when Four_of_a_kind =>
                  if Card_of_Count (Hystogram, 1) = Wild_Card then
                     Game_Store (G).Modified_Hand (Wild_Index) :=
                       Card_of_Count (Hystogram, 4);
                     Game_Store (G).Hand_Type := Five_of_a_kind;
                  else
                     Game_Store (G).Modified_Hand (Wild_Index) :=
                       Card_of_Count (Hystogram, 1);
                     Game_Store (G).Hand_Type := Full_House;
                  end if; -- Card_of_Count (Hystogram, 1) = Wild_Card
               when Full_house =>
                  if Card_of_Count (Hystogram, 3) = Wild_Card then
                     Game_Store (G).Modified_Hand (Wild_Index) :=
                       Card_of_Count (Hystogram, 2);
                  else
                     Game_Store (G).Modified_Hand (Wild_Index) :=
                       Card_of_Count (Hystogram, 3);
                  end if; -- Card_of_Count (Hystogram, 3) = Wild_Card
                  -- no change to Hand_Type
               when Three_of_a_kind =>
                  if Card_of_Count (Hystogram, 3) = Wild_Card then
                     Game_Store (G).Modified_Hand (Wild_Index) :=
                       Card_of_Count (Hystogram, 1);
                     Game_Store (G).Hand_Type := Two_pair;
                  else
                     Game_Store (G).Modified_Hand (Wild_Index) :=
                       Card_of_Count (Hystogram, 3);
                     Game_Store (G).Hand_Type := Four_of_a_kind;
                  end if; -- Card_of_Count (Hystogram, 3) = Wild_Card
               when Two_pair =>
                  if Card_of_Count (Hystogram, 1) = Wild_Card then
                     Game_Store (G).Modified_Hand (Wild_Index) :=
                       Card_of_Count (Hystogram, 2);
                     Game_Store (G).Hand_Type := Full_house;
                  else
                     Game_Store (G).Modified_Hand (Wild_Index) :=
                       Card_of_Count (Hystogram, 1);
                     -- no change to Hand_Type
                  end if; -- Card_of_Count (Hystogram, 1) = Wild_Card
               when One_pair =>
                  if Card_of_Count (Hystogram, 2) = Wild_Card then
                     Game_Store (G).Modified_Hand (Wild_Index) :=
                       Card_of_Count (Hystogram, 1);
                  else
                     Game_Store (G).Modified_Hand (Wild_Index) :=
                       Game_Store (G).Modified_Hand (Not_Wild_Index);
                  end if; -- Card_of_Count (Hystogram, 2) = Wild_Card
                  -- no change to Hand_Type
               when High_card =>Game_Store (G).Modified_Hand (Wild_Index) :=
                    Game_Store (G).Modified_Hand (Not_Wild_Index);
                  Game_Store (G).Hand_Type := One_Pair;
            end case; -- Game_Store (G).Modified_Hand_Type
            Build (Game_Store (G).Modified_Hand, Hystogram);
         end loop; -- Hystogram (Wild_Card) > 0
      end loop; -- G in Iterate (Game_Store)
   end Upgrade_Hand;

   Game_Store : Game_Stores.Vector;
   Sum : Natural := 0;

begin -- December_07
   Read_input (Game_Store);
   Characterise (Game_Store);
   Part_1_Sort.Sort (Game_Store);
   for G in Positive range 1 .. Last_Index (Game_Store) loop
      Sum := Sum + Game_Store (G).Bid * G;
   end loop; -- G in Positive range 1 .. Last_Index (Game_Store)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Sum := 0;
   Upgrade_Hand (Game_Store);
   Part_2_Sort.Sort (Game_Store);
   for G in Positive range 1 .. Last_Index (Game_Store) loop
      Sum := Sum + Game_Store (G).Bid * G;
      Put_Line (Player_States'Image (Game_Store (G)));
   end loop; -- G in Positive range 1 .. Last_Index (Game_Store)
   Put_Line ("Part two:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_07;
