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

procedure December_12_Part_1_Combinations is

   type Spring_States is (Operational, Damaged, Unknown);

   package Run_Lists is new Ada.Containers.Doubly_Linked_Lists (Positive);
   use Run_Lists;

   subtype Condition_Maps is Unbounded_String;

   type Condition_Reports is record
      Condition_Map : Condition_Maps;
      Run_List : Run_Lists.List;
   end record; -- Condition_Reports

   package Report_Stores is new
     Ada.Containers.Doubly_Linked_Lists (Condition_Reports);
   use Report_Stores;

   subtype Bits is Natural range 0 .. 31;
   -- allows for upto 32 unknown springs;
   package Perm_Maps is new Ada.Containers.Ordered_Maps (Bits, Positive);
   use Perm_Maps;

   procedure Read_input (Report_Store : out Report_Stores.list) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Spring_Set : constant Character_Set := To_Set (".#?");
      Condition_Report : Condition_Reports;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_12.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Report_Store);
      while not End_Of_File (Input_File) loop
         Clear (Condition_Report.Run_List);
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Spring_Set, Start_At, inside, First, Last);
         Condition_Report.Condition_Map := Unbounded_Slice (Text, First, Last);
         Start_At := Last + 1;
         loop -- read one data item
            Find_Token (Text, Decimal_Digit_Set, Start_At, inside, First, Last);
            Append (Condition_Report.Run_List,
                    Positive'Value (Slice (Text, First, Last)));
            Start_At := Last + 1;
            exit when Last = 0  or Start_At > Length (Text);
         end loop; -- read one data item
         Append (Report_Store, Condition_Report);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   function Count_Permutations (Condition_Report : in Condition_Reports)
                                return Natural is

      Count : Natural := 0;
      subtype Map_Indices is Positive range
        1 .. Length (Condition_Report.Condition_Map);
      type Maps is array (Map_Indices) of Spring_States;
      Map : Maps;
      Perm_Map : Perm_Maps.Map := Perm_Maps.Empty_Map;
      Unknown_Count : Bits := 0;
      Trial_List : Run_Lists.List;
      Bit_Mask : constant Unsigned_32 := 1;
      Run_Count : Natural := 0;

   begin -- Count_Permutations
      for M in Map_Indices loop
         case Element (Condition_Report.Condition_Map, M) is
            when '.' =>
               Map (M) := Operational;
            when '#' =>
               Map (M) := Damaged;
            when '?' =>
               Map (M) := Unknown;
               Include (Perm_Map, Unknown_Count, M);
               -- store the position of the unknown springs
               Unknown_Count := @ + 1;
            when others =>
               raise Program_Error with "Unexpected character '" &
                 Element (Condition_Report.Condition_Map, M) & "'";
         end case; -- Element (Condition_Report.Condition_Map, M)
      end loop; -- M in Map_Indices
      for Perm in Unsigned_32 range 1 .. 2 ** Unknown_Count loop
         for M in Iterate (Perm_Map) loop
            if (Perm and Shift_Left (Bit_Mask, Key (M))) = 0 then
               Map (Element (M)) := Operational;
            else
               Map (Element (M)) := Damaged;
            end if; -- (Perm and Shift_Left (Bit_Mask, Key (M))) = 0
         end loop; --  M in Iterate (Perm_Map);
         Clear (Trial_List);
         Run_Count := 0;
         for M in Map_Indices loop
            if Map (M) = Damaged then
               Run_Count := @ + 1;
               if M = Map_Indices'Last then
                  Append (Trial_List, Run_Count);
               end if; --Append (Trial_List, Run_Count);
            elsif  Map (M) = Operational then
               if Run_Count > 0 then
                  Append (Trial_List, Run_Count);
                  Run_Count := 0;
               end if; -- Run_Count > 0
            else
               raise Program_Error with "invalid map state, index:" & M'Img;
            end if; -- Map (M) = Unknown
         end loop; -- M in Map_Indices
         if Trial_List = Condition_Report.Run_List then
            Count := @ + 1;
         end if; -- Trial_List
      end loop; -- Perm in Unsigned_32 range 0 .. 2 ** Unknown_Count
      return Count;
   end Count_Permutations;

   Report_Store : Report_Stores.List;
   Result : Natural;
   Sum : Natural := 0;

begin -- December_12_Part_1_Combinations
   Read_input (Report_Store);
   for R in Iterate (Report_Store) loop
      Result := Count_Permutations (Element (R));
      Put_Line ("Result for " & Element (R).Condition_Map & Result'Img);
      Sum := @ + Result;
   end loop; -- R in Iterate (Report_Store)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_12_Part_1_Combinations;
