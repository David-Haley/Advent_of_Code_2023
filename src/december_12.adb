with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_12 is

   type Run_Indices is new Positive;
   package Run_Lists is new Ada.Containers.Vectors (Run_Indices, Positive);
   use Run_Lists;

   subtype Wells is Character with Static_Predicate => Wells in '.' | '#' | '?';
   subtype Known_Wells is Wells with
     Static_Predicate => Known_Wells in '.' | '#';

   type Map_Indices is new Positive;
   package Condition_Maps is new Ada.Containers.Vectors (Map_Indices, Wells);
   use Condition_Maps;

   type Condition_Reports is record
      Condition_Map : Condition_Maps.Vector := Condition_Maps.Empty_Vector;
      Run_List : Run_Lists.Vector := Run_Lists.Empty_Vector;
   end record; -- Condition_Reports

   package Report_Stores is new
     Ada.Containers.Vectors (Positive, Condition_Reports);
   use Report_Stores;

   subtype Runs is Natural;

   type Cache_Keys is record
      Well : Known_Wells; -- well value to be tested at current level.
      Map_Index : Map_Indices; -- current map element being considered
      Run_Index : Run_Indices; -- current element of run list being considered
      Run : Runs; -- current length of failed wells
   end record; -- Cache_Keys

   function "<" (Left, Right : Cache_Keys) return Boolean is
     (Left.Well < Right.Well or else
        (Left.Well = Right.Well and then (Left.Map_Index < Right.Map_Index
         or else (Left.Map_Index = Right.Map_Index and then
               (Left.Run_Index < Right.Run_Index
                or else (Left.Run_Index = Right.Run_Index and then
                  Left.Run < Right.Run))))));

   subtype Big_Natural is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   package Caches is new Ada.Containers.Ordered_Maps (Cache_Keys, Big_Natural);
   use Caches;

   procedure Read_input (Report_Store : out Report_Stores.Vector) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Condition_Report : Condition_Reports;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_12.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Report_Store);
      while not End_Of_File (Input_File) loop
         Clear (Condition_Report.Condition_Map);
         Clear (Condition_Report.Run_List);
         Get_Line (Input_File, Text);
         Start_At := 1;
         while Element (Text, Start_At) in Wells loop
            Append (Condition_Report.Condition_Map, Element (Text, Start_At));
            Start_At := @ + 1;
         end loop; -- Element (Text, Start_At) in Wells
         loop -- read one data item
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            Append (Condition_Report.Run_List,
                    Positive'Value (Slice (Text, First, Last)));
            Start_At := Last + 1;
            exit when Last = 0  or else Start_At > Length (Text);
         end loop; -- read one data item
         Append (Report_Store, Condition_Report);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   function Count_Combinations (Condition_Report : Condition_Reports)
                                return Big_Natural is

      function Search (Condition_Report : Condition_Reports;
                       Run_Index : Run_Indices;
                       Map_Index : Map_Indices;
                       Well : Known_Wells;
                       Run : Natural;
                       Cache : in out Caches.Map) return Big_Natural
        with pre =>
          Map_Index <= Last_Index (Condition_Report.Condition_Map) and then
             (Well = Condition_Report.Condition_Map (Map_Index) or else
                  Condition_Report.Condition_Map (Map_Index) = '?');

      function Search (Condition_Report : Condition_Reports;
                       Run_Index : Run_Indices;
                       Map_Index : Map_Indices;
                       Well : Known_Wells;
                       Run : Natural;
                       Cache : in out Caches.Map) return Big_Natural is

         -- Search space is constrained to match run list thus reducing the from
         -- 2 ** N where N is the number of wells of unknown state.

         Valid_Count : Big_Natural := 0;
         Cache_Key : constant Cache_Keys := (Well, Map_Index, Run_Index, Run);

      begin -- Search
         if Contains (Cache, Cache_Key) then
            return Cache (Cache_Key);
         end if; -- Contains (Cache, Cache_Key)
         if Map_Index = Last_Index (Condition_Report.Condition_Map) then
            case Well is
            when '.' =>
               if Run_Index > Last_Index (Condition_Report.Run_List) and then
                 Run = 0 then
                  Valid_Count := 1;
               end if; -- Run_Index > Last_Index (Condition_Report.Run_list) ...
            when '#' =>
               if Run_Index = Last_Index (Condition_Report.Run_List) and then
                 Condition_Report.Run_List (Run_Index) = Run then
                  Valid_Count := 1;
               end if; -- Run_Index = Last (Condition_Report.Run_List) ...
            end case; -- Well
         else
            case Well is
            when '.' =>
               if Run = 0 then
                  -- should always be true
                  case Condition_Report.Condition_Map (Map_Index + 1) is
                  when '.' =>
                     Valid_Count := @ +
                       Search (Condition_Report, Run_Index, Map_Index + 1,
                               '.', 0, Cache);
                  when '#' =>
                     Valid_Count := @ +
                       Search (Condition_Report, Run_Index, Map_Index + 1,
                               '#', 1, Cache);
                  when '?' =>
                     Valid_Count := @ +
                       Search (Condition_Report, Run_Index, Map_Index + 1,
                               '.', 0, Cache) +
                       Search (Condition_Report, Run_Index, Map_Index + 1,
                               '#', 1, Cache);
                  when others =>
                     raise Program_Error with "Bad well state after '.'" &
                       Condition_Report.Condition_Map (Map_Index + 1) &
                       "'";
                  end case; -- Condition_Report.Condition_Map (Map_Index + 1)
               end if; -- Run = 0
            when '#' =>
               if Run_Index <= Last_Index (Condition_Report.Run_List) and then
                 Condition_Report.Run_List (Run_Index) >= Run then
                  case Condition_Report.Condition_Map (Map_Index + 1) is
                  when '.' =>
                     if Run = Condition_Report.Run_List (Run_Index) then
                        Valid_Count := @ +
                          Search (Condition_Report, Run_Index + 1,
                                  Map_Index + 1, '.', 0, Cache);
                     end if; -- Run = Condition_Report.Run_List (Run_Index)
                  when '#' =>
                     Valid_Count := @ +
                       Search (Condition_Report, Run_Index, Map_Index + 1,
                               '#', Run + 1, Cache);
                  when '?' =>
                     if Run = Condition_Report.Run_List (Run_Index) then
                        Valid_Count := @ +
                          Search (Condition_Report, Run_Index + 1,
                                  Map_Index + 1, '.', 0, Cache);
                     end if; -- Run = Condition_Report.Run_List (Run_Index)
                     Valid_Count := @ +
                       Search (Condition_Report, Run_Index, Map_Index + 1,
                               '#', Run + 1, Cache);
                  when others =>
                     raise Program_Error with "Bad well state after '#'" &
                       Condition_Report.Condition_Map (Map_Index + 1) & "'";
                  end case; -- Condition_Report.Condition_Map (Map_Index + 1)
               end if; -- Run_Index <= last_Index (Condition_Report.Run_List ...
            end case; -- Well
         end if; -- Map_Index = Last_Index (Condition_Report.Condition_Map)
         Include (Cache, Cache_Key, Valid_Count);
         return Valid_Count;
      end Search;

      Cache : Caches.Map;

   begin -- Count_Combinations
      Clear (Cache);
      case Condition_Report.Condition_Map (1) is
         when '.' =>
            return Search (Condition_Report,
                           First_Index (Condition_Report.Run_List),
                           -- First element of Run_List
                           1, -- First index of Condition_Map
                           '.', -- First Well (unknown try good)
                           0,
                           Cache); -- Initial run length
         when '#' =>
            return Search (Condition_Report,
                           First_Index (Condition_Report.Run_List),
                           -- First element of Run_List
                           1, -- First index of Condition_Map
                           '#', -- First Well unknowmn try bad
                           1,
                           Cache); -- Initial run length
         when '?' =>
            return Search (Condition_Report,
                           First_Index (Condition_Report.Run_List),
                           -- First element of Run_List
                           1, -- First index of Condition_Map
                           '.', -- First Well (unknown try good)
                           0,
                           Cache) -- Initial run length
              + Search (Condition_Report,
                        First_Index (Condition_Report.Run_List),
                        -- First element of Run_List
                        1, -- First index of Condition_Map
                        '#', -- First Well unknowmn try bad
                        1,
                        Cache); -- Initial run length
         when others =>
            raise Program_Error with "Bad first well in map '" &
              Condition_Report.Condition_Map (1) & "'";
      end case; -- Element(Condition_Report).Condition_Map (1)
   end Count_Combinations;

   procedure Unfold (Report_Store : in Report_Stores.Vector;
                     Report_Store_Two : out Report_Stores.Vector) is

      Repeats : constant Positive := 5;
      Condition_Report : Condition_Reports;

   begin -- Unfold
      Clear (Report_Store_Two);
      for Rc in Iterate (Report_Store) loop
         Clear (Condition_Report.Condition_Map);
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

   Report_Store, Report_Store_Two : Report_Stores.Vector;
   Sum : Big_Natural := 0;

begin -- December_12
   Read_input (Report_Store);
   for R in Iterate (Report_Store) loop
      Sum := @ + Count_Combinations (Element (R));
   end loop; -- R in Iterate (Report_Store)
   Put_Line ("Part one:" & Sum'Img);
   Put_CPU_Time;
   Sum := 0;
   Unfold (Report_Store, Report_Store_Two);
   for R in Iterate (Report_Store_Two) loop
      Sum := @ + Count_Combinations (Element (R));
   end loop; -- R in Iterate (Report_Store)
   Put_Line ("Part two:" & Sum'Img);
   Put_CPU_Time;
end December_12;
