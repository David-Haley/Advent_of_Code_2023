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

   function Count_Combinations (Rc : in Condition_Reports.Cursor)
                                return Natural is

      function Search (Rc : in Condition_Reports.Cursor;
                       Run_C : in Run_Lists.Cursor;
                       Map_Index : in Positive;
                       Well : in Well;
                       Run : in Natural) return Natural is

         -- Search space is constrained to match run list thus reducing the from
         -- 2 ** N where N is the number of wells of unknown state.

         Valid_Count : Natural := 0;

      begin -- Search
         if Map_Index = Length (Element (Rc).Condition_Map) then
            case Element (Rc).Condition_Map (Map_Index) is
            when '.' =>
               if Well = '.' and -- should always be true
                 ((Run_C = No_Element and then Run = 0) or else
                      (Element (Rc).Run_List (Run_C) = Run and
                             Run_C = Last (Element (Rc).Run_List))) then
                  Valid_Count := @ + 1;
               end if; -- Well = '.' and ...
            when '#' =>
               if Well = '#' and -- should always be true
                 (Run_C = Last (Element (Rc).Run_List) and then
                  Element (Rc).Run_List (Run_C) = Run + 1) then
                  Valid_Count := @ + 1;
               end if; -- Well = '#' and ...
            when '?' =>
               if Well = '.' and
                 (Run_C = No_Element or else
                    (Element (Rc).Run_List (Run_C) = Run and
                         Run_C = Last (Element (Rc).Run_List))) then
                  Valid_Count := @ + 1;
               elsif Well = '#' and
                 (Run_C = Last (Element (Rc).Run_List) and then
                  Element (Rc).Run_List (Run_C) = Run + 1) then
                  Valid_Count := @ + 1;
               else
                  raise Program_Error with "End search, bad well state '" &
                    Well & "'";
               end if; -- Well = '.'
            when others =>
               raise Program_Error with "Bad well type in map, last element " &
                 Element (Element (Rc).Condition_Map,
                          Length(Element (Rc).Condition_Map));
            end case; -- Element (Rc).Condition_Map (Map_Index)
            return Valid_Count;
         elsif Map_Index < Length (Element (Rc).Condition_Map) then
            case Element (Rc).Condition_Map (Map_Index) is
            when '.' =>
               if Well = '.' and -- should always be true
                 ((Run_C = No_Element and then Run = 0) or else
                 Element (Rc).Run_List (Run_C) = Run) then
                  case Element (Rc).Condition_Map (Map_Index + 1) is
                  when '.' =>
                     Search (Rc, Run_C; Map_Index + 1, '.', 0);
                  when '#' =>
                     Valid_Count := @ +
                       Search (Rc, Next (Run_C), Map_Index + 1, '#', 0);
                  when '?' =>
                     Valid_Count := @ +
                       Search (Rc, Run_C, Map_Index + 1, '.', 0) +
                         Search (Rc, Next (Run_C), Map_Index + 1, '#', 1);
                  end case; -- Element (Rc).Condition_Map (Map_Index + 1)
               end if; -- Well = '.' and ..
            when '#' =>
               if Well = '#' and -- should always be true
                 (Run_C /= No_Element and then
                  Element (Rc).Run_List (Run_C) >= Run + 1) then
                  case Element (Rc).Condition_Map (Map_Index + 1) is
                  when '.' =>
                     Valid_Count := @ +
                       Search (Rc, Next (Run_C); Map_Index + 1, '.', Run);
                  when '#' =>
                     Valid_Count := @ +
                       Search (Rc, Next (Run_C); Map_Index + 1, '#', Run + 1);
                  when '?' =>
                     Valid_Count := @ +
                       Search (Rc, Next (Run_C); Map_Index + 1, '.', Run) +
                         Search (Rc, Next (Run_C); Map_Index + 1, '#', Run + 1);
                  end case; -- Element (Rc).Condition_Map (Map_Index + 1)
               end if; -- Well = '#' and ...
            when '?' =>
               if Well = '.' then
                  if (Run_C = No_Element and then Run = 0) or else
                    Element (Rc).Run_List (Run_C) = Run) then
                     case Element (Rc).Condition_Map (Map_Index + 1) is
                     when '.' =>
                        Valid_Count := @ +
                          Search (Rc, Next (Run_C); Map_Index + 1, '.', 0);
                     when '#' =>
                        Valid_Count := @ +
                          Search (Rc, Next (Run_C); Map_Index + 1, '#', 0);
                     when '?' =>
                        Valid_Count := @ +
                          Search (Rc, Next (Run_C); Map_Index + 1, '.', 0) +
                            Search (Rc, Next (Run_C); Map_Index + 1, '#', 0);
                     end case; -- Element (Rc).Condition_Map (Map_Index + 1)
                  end if; -- (Run_C = No_Element and then Run = 0) or else
               elsif Well = '#' then
                  if (Run_C /= No_Element and then
                      Element (Rc).Run_List (Run_C) >= Run + 1) then
                     case Element (Rc).Condition_Map (Map_Index + 1) is
                        when '.' =>
                           Valid_Count := @ +
                             Search (Rc, Next (Run_C); Map_Index + 1, '.', Run);
                        when '#' =>
                           Valid_Count := @ +
                             Search (Rc, Next (Run_C); Map_Index + 1, '#',
                                     Run + 1);
                        when '?' =>
                           Valid_Count := @ +
                             Search (Rc, Next (Run_C); Map_Index + 1, '.', Run)
                               + Search (Rc, Next (Run_C); Map_Index + 1, '#',
                                         Run + 1);
                     end case; -- Element (Rc).Condition_Map (Map_Index + 1)
                  end if; -- (Run_C /= No_Element and then
               else
                  raise Program_Error with "Bad Well " & Well;
               end if; -- Well = '#'
            when others =>
               raise Program_Error with "Bad well type in map " &
                 Element (Element (Rc).Condition_Map,
                          Length(Element (Rc).Condition_Map)) &
                 " at index:" & Map_Index'Img;
            end case; -- Element (Rc).Condition_Map (Map_Index)
            end case; -- Element (Rc).Condition_Map (Map_Index)
         else
            raise Program_Error with "Search beyond end of map, Map_Index:" &
              Map_Index'Img
         end if; -- Map_Index = Length (Element (Rc).Condition_Map)
      end Search;

   begin -- Count_Combinations
      return Search (Rc,
                     First (Element(Rc).Run_List), -- First element of Run_List
                     1, -- First element of Condition_Map
                     Element (Element(Rc).Condition_Map, 1), -- First Well
                     0) -- Initial run length
   end Count_Combinations;

   Report_Store : Report_Stores.List;
   Sum : Natural := 0;

begin -- December_12
   Read_input (Report_Store);
   for R in Iterate (Report_Store) loop
      Sum := @ + Count_Combinations (R);
   end loop; -- R in Iterate (Report_Store)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_12;
