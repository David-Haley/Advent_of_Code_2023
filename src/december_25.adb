with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Interfaces; use Interfaces;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_25 is

   subtype Component_Names is String (1 .. 3);

   package Neighbours is new Ada.Containers.Ordered_Sets (Component_Names);
   use Neighbours;

   package Connection_Maps is new
     Ada.Containers.Ordered_Maps (Component_Names, Neighbours.Set);
   use Connection_Maps;

   type Connections is record
      Node_1, Node_2 : Component_Names;
   end record; -- Connections

   function "<" (Left, Right : Connections) return Boolean is
      ((Left.Node_1 & Left.Node_2) < (Right.Node_1 & Right.Node_2));

   package Connection_Sets is new Ada.Containers.Ordered_Sets (Connections);
   use Connection_Sets;

   package Link_Lists is new
     Ada.Containers.Ordered_Maps (Connections, Connection_Sets.Set);
   use Link_Lists;

   Function Make_Key (Left, Right : in Component_Names) return Connections is

   begin -- Make_Key
      if Left < Right then
         return (Left, Right);
      else
         return (Right, Left);
      end if; -- Left < Right
   end Make_Key;

   procedure Read_input (Connection_Map : out Connection_Maps.Map) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : natural;
      Delimiters : constant Character_Set := To_Set (" :");
      Key : Component_Names;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_25.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Connection_Map);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Put_Line (Text);
         Start_At := 1;
         Find_Token (Text, Delimiters, Start_at, Outside, First, Last);
         Key := Slice (Text, First, Last);
         Insert (Connection_Map, Key, Neighbours.Empty_Set);
         Start_At := Last + 1;
         loop -- Read one Neighbour;
            Find_Token (Text, Delimiters, Start_at, Outside, First, Last);
            exit when Last = 0;
            Insert (Connection_Map (Key), Slice (Text, First, Last));
            Start_At := Last + 1;
         end loop; -- Read one Neighbour;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   procedure Complete_Map (Connection_Map : in out Connection_Maps.Map) is

      Cc : Connection_Maps.Cursor := First (Connection_Map);

   begin -- Complete_Map
      while Cc /= Connection_Maps.No_Element loop
         for N in Iterate (Element (Cc)) loop
            if not Contains (Connection_Map, Element (N)) then
               Insert (Connection_Map, Element (N), Neighbours.Empty_Set);
            end if; -- not Contains (Connection_Map, Element (N))
            include (Connection_Map (Element (N)), Key (Cc));
         end loop; -- N in Iterate (Element (Cc))
         Next (Cc);
      end loop; -- Cc /= Connection_Maps.No_Element
   end Complete_Map;

   procedure Build (Connection_Map : in Connection_Maps.Map;
                    Link_List : in out Link_Lists.Map) is

      package Visited_Lists is new
        Ada.Containers.Doubly_Linked_Lists (Component_Names);
      use Visited_Lists;

      procedure Find (Source, Destination : Component_Names;
                      Connection_Map : in Connection_Maps.Map;
                      Link_List : in out Link_Lists.Map;
                      Visited_List : in out Visited_Lists.List) is

         V2 : Visited_Lists.Cursor;

      begin -- Find
         if Destination < Source then
            raise Program_Error with "not correctly ordered: " & Source & ", "
              & Destination;
         end if; -- Destination < Source
         if Destination = Last_Element (Visited_List) then
            -- destination found store results
            for V1 in Iterate (Visited_List) loop
               V2 := Next (V1);
               if V2 /= Visited_Lists.No_Element then
                  if not Contains (Link_List,
                                   Make_Key (Element (V1), Element (V2))) then
                     Insert (Link_List,
                             Make_Key (Element (V1), Element (V2)),
                             Connection_Sets.Empty_Set);
                  end if; -- not Contains (Link_List,
                  Include (Link_List (Make_Key (Element (V1), Element (V2))),
                           (Source, Destination));
               end if; -- V2 /= Visited_Lists.No_Element
            end loop; -- V1 in Iterate (Visited_List)
         else
            -- Continue search
            for N in Iterate (Connection_Map (Last_Element (Visited_List))) loop
               Append (Visited_List, Element (N));
               Find (Source, Destination, Connection_Map, Link_Lists,
                     Visited_List);
               Delete_Last (Visited_List);
            end loop; -- N in Iterate (Connection_Map (Last_Element ...
         end if; -- Destination = Last_Element (Visited_List)
      end Find;

      C2 : Connection_Maps.Cursor;
      Visited_List : Visited_Lists;

   begin -- Build
      for C1 in Iterate (Connection_Map) loop
         Clear (Visited_List);
         C2 := Next (C1);
         while C2 /= Connection_Maps.No_Element loop
            Find (Key (C1), Key (C2), Connection_Map, Link_Lists, Visited_List);
         end loop; -- C2 /= Connection_Maps.No_Element
      end loop; -- C1 in Iterate (Connection_Map)
   end Build;

   Connection_Map : Connection_Maps.Map;

begin -- December_25
   Read_input (Connection_Map);
   Put_Line (Connection_Map'Img);
   Complete_Map (Connection_Map);
   Put_Line (Connection_Map'Img);
   Put_Line ("Part one:");
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_25;
