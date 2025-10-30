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
with DJH.Date_and_Time_Strings; use DJH.Date_and_Time_Strings;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_12 is

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

   subtype Wells is Character with Static_Predicate => Wells in '.' | '#' | '?';
   subtype Known_Wells is Wells with
     Static_Predicate => Known_Wells in '.' | '#';

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

   function Count_Combinations (Rc : in Report_Stores.Cursor)
                                return Natural is

      function Search (Rc : in Report_Stores.Cursor;
                       Run_C : in Run_Lists.Cursor;
                       Map_Index : in Positive;
                       Well : in Known_Wells;
                       Run : in Natural) return Natural
        with pre =>
          (Well = Element (Element (RC).Condition_Map, Map_index) or
                    Element (Element (RC).Condition_Map, Map_index) = '?') and
                  Map_index <= Length (Element (RC).Condition_Map);


      function Search (Rc : in Report_Stores.Cursor;
                       Run_C : in Run_Lists.Cursor;
                       Map_Index : in Positive;
                       Well : in Known_Wells;
                       Run : in Natural) return Natural is

         -- Search space is constrained to match run list thus reducing the from
         -- 2 ** N where N is the number of wells of unknown state.

         Valid_Count : Natural := 0;

      begin -- Search
         if Map_Index = Length (Element (Rc).Condition_Map) then
            case Well is
            when '.' =>
               if Run_C = Run_lists.No_Element and Run = 0 then
                  Valid_Count := 1;
               end if; -- Well = '.' and ...
            when '#' =>
               if (Run_C /= Run_Lists.No_Element and
                     Next (Run_C) = Run_Lists.No_Element) and then
                 Element (Run_C) = Run then
                  Valid_Count := 1;
               end if; -- (Run_C = Last (Element (Rc).Run_List) and then ...
            end case; -- Well
         else
            case Well is
            when '.' =>
               --  Put_Line ("More map to process good well");
               if Run = 0 then
                  -- should always be true
                  case Element (Element (Rc).Condition_Map, Map_Index + 1) is
                  when '.' =>
                     Valid_Count := @ +
                       Search (Rc, Run_C, Map_Index + 1, '.', 0);
                  when '#' =>
                     Valid_Count := @ +
                       Search (Rc, Run_C, Map_Index + 1, '#', 1);
                  when '?' =>
                     Valid_Count := @ +
                       Search (Rc, Run_C, Map_Index + 1, '.', 0) +
                       Search (Rc, Run_C, Map_Index + 1, '#', 1);
                  when others =>
                     raise Program_Error with "Bad well state after '.'" &
                       Element (Element (Rc).Condition_Map, Map_Index + 1) &
                       "'";
                  end case; -- Element (Element (Rc).Condition_Map, Map_Index + 1)
               end if; -- Well = '.' and Run = 0
            when '#' =>
               --  Put_Line ("More map to process bad well");
               if (Run_C /= Run_Lists.No_Element and then
                   Element (Run_C) >= Run) then
                  case Element (Element (Rc).Condition_Map, Map_Index + 1) is
                  when '.' =>
                     if Run = Element (Run_C) then
                        Valid_Count := @ +
                          Search (Rc, Next (Run_C), Map_Index + 1, '.', 0);
                     end if; -- Run = Element (Run_C)
                  when '#' =>
                     Valid_Count := @ +
                       Search (Rc, Run_C, Map_Index + 1, '#', Run + 1);
                  when '?' =>
                     if Run = Element (Run_C) then
                        Valid_Count := @ +
                          Search (Rc, Next (Run_C), Map_Index + 1, '.', 0);
                     end if; -- Run = Element (Run_C)
                     Valid_Count := @ +
                         Search (Rc, Run_C, Map_Index + 1, '#', Run + 1);
                  when others =>
                     raise Program_Error with "Bad well state after '#'" &
                       Element (Element (Rc).Condition_Map, Map_Index + 1) &
                       "'";
                  end case; -- Element (Element (Rc).Condition_Map, Map_Index + 1)
               end if; --  (Run_C /= Run_Lists.No_Element and then
            end case; -- Well
         end if; -- Map_Index = Length (Element (Rc).Condition_Map)
         return Valid_Count;
      end Search;

   begin -- Count_Combinations
      case Element (Element(Rc).Condition_Map, 1) is
         when '.' =>
            return Search (Rc,
                           First (Element(Rc).Run_List),
                           -- First element of Run_List
                           1, -- First index of Condition_Map
                           '.', -- First Well (unknown try good)
                           0); -- Initial run length
         when '#' =>
            return Search (Rc,
                           First (Element(Rc).Run_List),
                           -- First element of Run_List
                           1, -- First index of Condition_Map
                           '#', -- First Well unknowmn try bad
                           1); -- Initial run length
         when '?' =>
            return Search (Rc,
                           First (Element(Rc).Run_List),
                           -- First element of Run_List
                           1, -- First index of Condition_Map
                           '.', -- First Well (unknown try good)
                           0) -- Initial run length
              + Search (Rc,
                        First (Element(Rc).Run_List),
                        -- First element of Run_List
                        1, -- First index of Condition_Map
                        '#', -- First Well unknowmn try bad
                        1); -- Initial run length
         when others =>
            raise Program_Error with "Bad first well in map '" &
              Element (Element(Rc).Condition_Map, 1) & "'";
      end case; -- Element (Element(Rc).Condition_Map, 1)
   end Count_Combinations;

   procedure Unfold (Report_Store : in Report_Stores.List;
                     Report_Store_Two : out Report_Stores.List) is

      Repeats : constant Positive := 5;
      Condition_Report : Condition_Reports;

   begin -- Unfold
      Clear (Report_Store_Two);
      for Rc in Iterate (Report_Store) loop
         Condition_Report.Condition_Map := Null_Unbounded_String;
         Clear (Condition_Report.Run_List);
         for F in Positive range 1 .. Repeats loop
            Append (Condition_Report.Condition_Map, Element (Rc).Condition_Map);
            if F < Repeats then
               Append (Condition_Report.Condition_Map, '?');
            end if; -- F < Repeats
            for Run in Iterate (Report_Store (Rc).Run_List) loop
               Append (Condition_Report.Run_List, Element (Run));
            end loop; -- Run in Iterate (Element (Rc).Run_List)
         end loop; -- F in Positive range 1 .. Repeats
         Append (Report_Store_Two, Condition_Report);
      end loop; -- Rc in Iterate (Report_Store)
   end Unfold;

   Report_Store, Report_Store_Two : Report_Stores.List;
   Sum : Natural := 0;
   Sum_two : Long_Long_Integer := 0;
   Count : Natural := 0;

begin -- December_12
   Read_input (Report_Store);
   for R in Iterate (Report_Store) loop
      Sum := @ + Count_Combinations (R);
   end loop; -- R in Iterate (Report_Store)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Unfold (Report_Store, Report_Store_Two);
   for R in Iterate (Report_Store_Two) loop
      Sum_Two := @ + Long_Long_integer (Count_Combinations (R));
      Count := @ + 1;
      Put_Line ("Processed:" & ' ' & Time_String & Count'Img & Sum_Two'Img);
   end loop; -- R in Iterate (Report_Store_Two)
   Put_Line ("Part two:" & Sum_two'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_12;
