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

   -- Solution based on 4Hbq posting on Reddit

   subtype Component_Names is String (1 .. 3);

   package Neighbours is new Ada.Containers.Ordered_Sets (Component_Names);
   use Neighbours;

   package Connection_Maps is new
     Ada.Containers.Ordered_Maps (Component_Names, Neighbours.Set);
   use Connection_Maps;

   type Disjoint_Sets is array (Boolean) of Neighbours.Set;

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

   procedure Initial_Split (Connection_Map : in Connection_Maps.Map;
                            Disjoint_Set : out Disjoint_Sets) is

      S : Boolean := True;

   begin -- Initial_Split
      Disjoint_Set := (others => Neighbours.Empty_Set);
      for C in Iterate (Connection_Map) loop
         insert (Disjoint_Set (S), Key (C));
         S := not S;
      end loop; -- C in Iterate (Connection_Map)
   end Initial_Split;

   procedure Split (Connection_Map : in Connection_Maps.Map;
                    Disjoint_Set : in out Disjoint_Sets) is

      function External_Count (Component : in Component_names;
                               Connection_Map : in Connection_Maps.Map;
                               Disjoint_Set : in Disjoint_Sets)
                               return Natural is

         Count : Natural := 0;

      begin -- External_Count
         if Contains (Disjoint_Set (False), Component) then
            for N in Iterate (Connection_Map (Component)) loop
               if Contains (Disjoint_Set (True), Element (N)) then
                  Count := @ + 1;
               end if; -- Contains (Disjoint_Set (True), Element (N))
            end loop; -- N in Iterate (Connection_Map (Component))
         else
            for N in Iterate (Connection_Map (Component)) loop
               if Contains (Disjoint_Set (False), Element (N)) then
                  Count := @ + 1;
               end if; -- Contains (Disjoint_Set (False), Element (N))
            end loop; -- N in Iterate (Connection_Map (Component))
         end if; -- Contains (Disjoint_Set (False), Component)
         return Count;
      end External_Count;

      function Connections (Connection_Map : in Connection_Maps.Map;
                            Disjoint_Set : in Disjoint_Sets)
                            return Neighbours.set is

         Result : Neighbours.Set := Neighbours.Empty_Set;

      begin -- Connections
         for S in Boolean loop
            for M in Iterate (Disjoint_Set (S)) loop
               for N in Iterate (Connection_Map (Element (M))) loop
                  If Contains (Disjoint_Set (not S), Element (N)) then
                     Include (Result, Element (N));
                  end if; -- Contains (Disjoint_Set (not S), Element (N))
               end loop; -- N in Iterate (Connection_Map (Element (M)))
            end loop; -- M in Iterate (Disjoint_Set (S))
         end loop; -- S in Boolean
         return Result;
      end Connections;

      Nc, Next_Nc : Neighbours.Cursor;
      Max_Count : Natural;

   begin -- Split
      while Length (Connections (Connection_Map, Disjoint_Set)) /= 3 loop
         for S in Boolean loop
            Max_Count := 2;
            for N in Iterate (Disjoint_Set (S)) loop
               if Max_Count < External_Count (Element (N), Connection_Map,
                                              Disjoint_Set) then
                  Max_Count := External_Count (Element (N), Connection_Map,
                                               Disjoint_Set);
               end if; -- Max_Count < External_Count (Element (N) ...
            end loop; -- N in Iterate (Disjoint_Set (S)) loop
            Nc := First (Disjoint_Set (S));
            while Nc /= Neighbours.No_Element loop
               if External_Count (Element (Nc), Connection_Map, Disjoint_Set) =
                 Max_Count then
                  Put (Element (Nc) & " ");
                  insert (Disjoint_Set (not S), Element (Nc));
                  Next_Nc := Next (Nc);
                  Delete (Disjoint_Set (S), Nc);
                  Nc := Next_Nc;
               else
                  Next (Nc);
               end if; -- External_Count (Element (Nc), Connection_Map ...
            end loop; -- Nc /= Neighbours.No_Element
            if S then
               Put_Line ("<");
            else
               Put_Line (">");
            end if; -- S
            Put_Line ("Connections: " & Connections (Connection_Map, Disjoint_Set)'Img);
            Put_Line (Length (Disjoint_Set (False))'Img & Length (Disjoint_Set (True))'Img);
         end loop; -- S in Boolean
      end loop; -- Length (Connections (Connection_Map, Disjoint_Set)) /= 3
   end Split;

   Connection_Map : Connection_Maps.Map;
   Disjoint_Set : Disjoint_Sets;

begin -- December_25
   Read_input (Connection_Map);
   Complete_Map (Connection_Map);
   for C in Iterate (Connection_Map) loop
      Put_Line (Key (C) & Length (Element (C))'Img);
   end loop;
   Initial_Split (Connection_Map, Disjoint_Set);
   Split (Connection_Map, Disjoint_Set);
   Put_Line (Disjoint_Set'Img);
   Put_Line ("Part one:" & Count_Type'Image (Length (Disjoint_Set (False)) *
               Length (Disjoint_Set(True))));
   DJH.Execution_Time.Put_CPU_Time;
end December_25;
