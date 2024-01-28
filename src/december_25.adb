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
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
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

   type Edges is record
      Component_1, Component_2 : Component_Names;
   end record; -- Edges;

   function "<" (Left, Right : Edges) return Boolean is
     (Left.Component_1 & Left.Component_2 <
        Right.Component_1 & Right.Component_2);

   package Visited_Sets is new Ada.Containers.Ordered_Sets (Edges);
   use Visited_Sets;

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

   function To_Edge (Left, Right : in Component_Names) return Edges is

   begin -- To_Edge Make_Edge
      if Left < Right then
         return (Left, Right);
      else
         return (Right, Left);
      end if; -- Make_Edge
   end To_Edge;

   pragma Inline_Always (To_Edge);

   function Find_Last (Start : in Component_Names;
                       Connection_Map : in Connection_Maps.Map)
                       return Component_Names is

      package Q_I is new
        Ada.Containers.Synchronized_Queue_Interfaces (Component_Names);
      use Q_I;

      package Queues is new Ada.Containers.Unbounded_Synchronized_Queues (Q_I);
      use Queues;

      Visited_Set : Visited_Sets.Set;
      Queue : Queues.Queue;
      Current : Component_Names;

   begin -- Find_Last
      Clear (Visited_Set);
      Queue.Enqueue (Start);
      while Queue.Current_Use > 0 loop
         Queue.Dequeue (Current);
         for N in Iterate (Connection_Map (Current)) loop
            if not Contains (Visited_Set, To_Edge (Current, Element (N))) then
               include (Visited_Set, To_Edge (Current, Element (N)));
               Queue.Enqueue (Element (N));
            end if; -- not Contains (Visited_Set, To_Edge (Current, ...
         end loop; -- N in Iterate (Connection_Map (Current))
      end loop; -- Queue.Current_Use > 0
      return Current;
   end Find_Last;

   procedure Find_First (Start, Destination : in Component_Names;
                         Visited_Set_In : In Visited_Sets. Set;
                         Connection_Map : in Connection_Maps.Map;
                         Found : out Boolean;
                         Path : out Visited_Sets.Set;
                         Reachable : out Neighbours.Set) is

      type Search_Elements is record
         Component_Name : Component_Names;
         Path : Visited_Sets.Set;
      end record; -- Search_Elements

      package Q_I is new
        Ada.Containers.Synchronized_Queue_Interfaces (Search_Elements);
      use Q_I;

      package Queues is new Ada.Containers.Unbounded_Synchronized_Queues (Q_I);
      use Queues;

      Visited_Set : Visited_Sets.Set := Copy (Visited_Set_in);
      Queue : Queues.Queue;
      Current, Next : Search_Elements := (Start, Visited_Sets.Empty_Set);

   begin -- Find_First
      Clear (Path);
      Reachable := To_Set (Start);
      Found := False;
      Queue.Enqueue (Current);
      while not Found and Queue.Current_Use > 0 loop
         Queue.Dequeue (Current);
         Found := Current.Component_Name = Destination;
         if Found then
            Path := Current.Path;
         else
            for N in Iterate (Connection_Map (Current.Component_Name)) loop
               if not Contains (Visited_Set,
                                To_Edge (Current.Component_Name,
                                  Element (N))) then
                  Include (Reachable, (Element (N)));
                  Include (Visited_Set,
                           To_Edge (Current.Component_Name, Element (N)));
                  Next := (Element (N), Copy (Current.Path));
                  Include (Next.Path,
                           To_Edge (Current.Component_Name, Element (N)));
                  Queue.Enqueue (Next);
               end if; -- not Contains (Visited_Set, To_Edge (Current, ...
            end loop; -- N in Iterate (Connection_Map (Current.Component_Name))
         end if; -- Found
      end loop; -- not Found and Queue.Current_Use > 0
   end Find_First;

   Connection_Map : Connection_Maps.Map;
   Left, Right : Component_Names;
   Path, Visited : Visited_Sets.Set;
   Reachable : Neighbours.Set;
   Found : Boolean;

begin -- December_25
   Read_input (Connection_Map);
   Complete_Map (Connection_Map);
   Left := Find_Last (First_Key (Connection_Map), Connection_Map);
   --  Left := Find_Last ("pzl", Connection_Map);
   Right := Find_Last (left, Connection_Map);
   Put_Line (First_Key (Connection_Map) & " " & Left & " " & Right);
   -- Left and Right are most distant from each other and in the two distinct
   -- groups.
   Clear (Visited);
   Find_First (Left, Right, Visited, Connection_Map, Found, Path, Reachable);
   if Found then
      Union (Visited, Path);
   else
      Put_Line (Reachable'Img);
      raise Program_Error with Right & " not found path 1";
   end if; -- Found
   Find_First (Left, Right, Visited, Connection_Map, Found, Path, Reachable);
   if Found then
      Union (Visited, Path);
   else
      Put_Line (Reachable'Img);
      raise Program_Error with Right & " not found path 2";
   end if; -- Found
   Find_First (Left, Right, Visited, Connection_Map, Found, Path, Reachable);
   if Found then
      Union (Visited, Path);
   else
      Put_Line (Reachable'Img);
      raise Program_Error with Right & " not found path 3";
   end if; -- Found
   Find_First (Left, Right, Visited, Connection_Map, Found, Path, Reachable);
   if Found then
      raise Program_Error with Right & " only three paths expected";
   end if; -- Found
   Put_Line (Reachable'Img);
   Put_Line ("Part one:" & Count_Type'Image (Length (Reachable) *
             (Length (Connection_Map) - Length (Reachable))));
   DJH.Execution_Time.Put_CPU_Time;
   --  Find_First (Right, Left, Visited, Connection_Map, Found, Path, Reachable);
   --  if Found then
   --     raise Program_Error with Right & " only three paths expected";
   --  end if; -- Found
   --  Put_Line (Reachable'Img);
   --  Reachable := Connection_Map ("pzl");
   --  Put_Line (Reachable'Img);
end December_25;
