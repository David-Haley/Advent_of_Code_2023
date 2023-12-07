with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_05 is

   subtype Property_Values is
     Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   package Property_Lists is new
      Ada.Containers.Doubly_Linked_Lists (Property_Values);
   use Property_Lists;

   subtype Properties is Unbounded_String;

   type Mapping_Elements is record
      Destination, Source, Map_Range : Property_Values;
   end record; -- Mapping_Elements

   package Mapping_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Mapping_Elements);
   use Mapping_Lists;

   type Mappings is record
      Destination : Properties;
      Mapping_List : Mapping_Lists.List;
   end record; -- Mappings

   package Property_Maps is new
     Ada.Containers.Ordered_Maps (Properties, Mappings);
   use Property_Maps;

   procedure Read_input (Seed_List : out Property_Lists.List;
                         Property_Map : out Property_Maps.Map) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      From_Property, To_Property : Properties;
      Mapping : Mapping_Elements;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_05.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Seed_List);
      Clear (Property_Map);
      Get_Line (Input_File, Text);
      Start_At := 1;
      loop -- read one seed
         Find_Token (Text, Decimal_Digit_Set, Start_At, inside, First, Last);
         exit when Last = 0;
         Start_At := Last + 1;
         Append (Seed_List, Property_Values'Value (Slice (Text, First, Last)));
      end loop; -- read one seed
      Get_Line (Input_File, Text);
      while not End_Of_File (Input_File) loop
         if Length (Text) = 0 then
            Get_Line (Input_File, Text);
            Start_At := 1;
            Find_Token (Text, Lower_Set, Start_At, inside, First, Last);
            From_Property := Unbounded_Slice (Text, First, Last);
            Start_At := Last + 1;
            Find_Token (Text, Lower_Set, Start_At, inside, First, Last);
            -- skip "-to-"
            Start_At := Last + 1;
            Find_Token (Text, Lower_Set, Start_At, inside, First, Last);
            To_Property := Unbounded_Slice (Text, First, Last);
            include (Property_Map, From_Property, (To_Property,
                     Mapping_Lists.Empty_List));
         end if; -- Length (Text) = 0
         loop -- get one mapping
            Get_Line (Input_File, Text);
            exit when Length (Text) = 0 or End_Of_File (Input_File);
            Start_At := 1;
            Find_Token (Text, Decimal_Digit_Set, Start_At, inside, First, Last);
            Mapping.Destination :=
              Property_Values'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
            Find_Token (Text, Decimal_Digit_Set, Start_At, inside, First, Last);
            Mapping.Source :=
              Property_Values'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
            Find_Token (Text, Decimal_Digit_Set, Start_At, inside, First, Last);
            Mapping.Map_Range :=
              Property_Values'Value (Slice (Text, First, Last));
            Append (Property_Map (From_Property).Mapping_List, Mapping);
         end loop; -- get one mapping
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   function Mapper (Mapping_List : in Mapping_Lists.List;
                    Value : in Property_Values) return Property_Values is

      Found : Boolean := False;
      Mc : Mapping_Lists.Cursor := First (Mapping_List);
      Result : Property_Values;


   begin -- Mapper
      while not Found and Mc /= Mapping_Lists.No_Element loop
         if Element (Mc).Source <= Value and
           Value < Element (Mc).Source + Element (Mc).Map_Range then
            Result := Value - Element (Mc).Source + Element (Mc).Destination;
           Found := True;
         end if; -- Element (Mc).Source <= Value and ..
         Next (Mc);
      end loop; -- not Found and Mc /= Mapping_Lists.No_Element
      if Found then
         Return Result;
      else
         return Value;
      end if; -- Found
   end Mapper;

   function Seed_to_Location (Property_Map : in Property_Maps.Map;
                              Seed : in Property_Values)
                              return Property_Values is

      Current_Property : Properties := To_Unbounded_String ("seed");
      Result : Property_Values := Seed;

   begin -- Seed_to_Location
      while Current_Property /= "location" loop
         Result :=
           Mapper (Property_Map (Current_Property).Mapping_List, Result);
         Current_Property := Property_Map (Current_Property).Destination;
      end loop; -- Current_Property /= "location"
      return Result;
   end Seed_to_Location;

   Seed_List : Property_Lists.List;
   Property_Map : Property_Maps.Map;
   Least : Property_Values;
   Sc : Property_Lists.Cursor;

begin -- December_05
   Read_input (Seed_List, Property_Map);
   Least := Property_Values'Last;
   for S in Iterate (Seed_List) loop
      if Seed_to_Location (Property_Map, Element (S)) < Least then
         Least := Seed_to_Location (Property_Map, Element (S));
      end if; -- Seed_to_Location (Property_Map, Element (S)) < Least
   end loop; -- S in Iterate (Seed_List)
   Put_Line ("Part one:" & Least'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Sc := First (Seed_List);
   Least := Property_Values'Last;
   while Sc /= Property_Lists.No_Element loop
      for S in Property_Values Range Seed_List (Sc) ..
        Seed_List (Sc) + Seed_List (Next(Sc)) - 1 loop
         if Seed_to_Location (Property_Map, S) < Least then
            Least := Seed_to_Location (Property_Map, S);
         end if; -- Seed_to_Location (Property_Map, S) < Least
      end loop; -- S in Property_Values Range Seed_List (Sc) ...
      Next (Sc);
      Next (Sc);
   end loop; -- Sc /= Property_Lists.No_Element
   Put_Line ("Part two:" & Least'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_05;
