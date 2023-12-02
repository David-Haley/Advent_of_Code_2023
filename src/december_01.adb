with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_01 is

   subtype Two_Digits is Natural range 0 .. 99;

   subtype Decimal_Characters is Character range '1' .. '9';
   type Name_Tables is array (Decimal_Characters) of Unbounded_String;

   package Two_Digit_Stores is new
     Ada.Containers.Doubly_Linked_Lists (Two_Digits);
   use Two_Digit_Stores;

   procedure Read_input (Two_Digit_Store : out Two_Digit_stores.List;
                         Part_Two : Boolean := False) is

      procedure Spelt (Text : in Unbounded_String;
                       First : out Positive;
                       First_Character : out Decimal_Characters;
                       Last : out Natural;
                       Last_Character : out Decimal_Characters) is

         Name_Table : constant Name_Tables :=
           (To_Unbounded_String ("one"), To_Unbounded_String ("two"),
            To_Unbounded_String ("three"), To_Unbounded_String ("four"),
            To_Unbounded_String ("five"), To_Unbounded_String ("six"),
            To_Unbounded_String ("seven"), To_Unbounded_String ("eight"),
            To_Unbounded_String ("nine"));

      begin -- Spelt_First
         -- Return value if none found;
         First := Length (Text) + 1;
         First_Character := Decimal_Characters'First;
         Last := 0;
         Last_Character := Decimal_Characters'First;
         for D in Decimal_Characters loop
            if Index (Text, To_String (Name_Table (D))) > 0 and then
              Index (Text, To_String (Name_Table (D))) < First then
               First := Index (Text, To_String (Name_Table (D)));
               First_Character := D;
            end if; -- Index (Text, To_String (Name_Table (D))) > 0 and then ...
            if Index (Text, To_String (Name_Table (D)), Backward) > Last then
               Last := Index (Text, To_String (Name_Table (D)), Backward);
               Last_Character := D;
            end if; -- Index (Text, To_String (Name_Table (D)) Backward) > ...
         end Loop; -- D in Decimal_Characters
      end Spelt;

      Input_File : File_Type;
      Text : Unbounded_String;
      Two_Digit_String : String (1 .. 2);
      Start_At, First, Digit_First : Positive;
      Last, Digit_Last : Natural;
      First_Spelt, Last_Spelt : Decimal_Characters;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_01.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Two_Digit_Store);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Start_At := Last + 1;
         if Last > 0 then
            Digit_First := First;
         else
            Digit_First := Length (Text) + 1;
         end if; -- Last > 0
         Digit_Last := Last;
         loop -- Search Again
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            exit when Last = 0;
            Digit_Last := Last;
            Start_At := Last + 1;
         end loop; -- Search Again;
         Spelt (Text, First, First_Spelt, Last, Last_Spelt);
         if Part_Two and First < Digit_First then
            Two_Digit_String (1) := First_Spelt;
         elsif Digit_First <= Length (Text) then
            Two_Digit_String (1) := Element (Text, Digit_First);
         else
            -- required to run second example for part 1
            Two_Digit_String (1) := '0';
         end if; -- Part_Two and First < Digit_First
         if Part_Two and Last > Digit_Last then
            Two_Digit_String (2) := Last_Spelt;
         elsif Digit_Last > 0 then
            Two_Digit_String (2) := Element (Text, Digit_Last);
         else
            -- required to run second example for part 1
            Two_Digit_String (2) := '0';
         end if; -- Part_Two and Last > Digit_Last
         append (Two_Digit_Store, Two_Digits'Value (Two_Digit_String));
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   Two_Digit_Store : Two_Digit_stores.List;
   Sum : Natural;

begin -- December_01
   Read_input (Two_Digit_Store);
   Sum := 0;
   for T in Iterate (Two_Digit_Store) loop
      Sum := Sum + Element (T);
   end loop; -- T in Iterate (Two_Digit_Store)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Read_input (Two_Digit_Store, True);
   Sum := 0;
   for T in Iterate (Two_Digit_Store) loop
      Sum := Sum + Element (T);
   end loop; -- T in Iterate (Two_Digit_Store)
   Put_Line ("Part two:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_01;
