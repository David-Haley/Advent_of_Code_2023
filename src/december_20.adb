with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_20 is

   subtype Pulses is Boolean;

   High : constant Pulses := True;

   Low : constant Pulses := False;

   subtype Gate_Names is Unbounded_String;

   type Gates is (broadcaster, Flip_Flop, Conjunction, Output);

   package Input_Maps is new Ada.Containers.Ordered_Maps (Gate_Names, Pulses);
   use Input_Maps;

   package Output_Lists is new Ada.Containers.Doubly_Linked_Lists (Gate_Names);
   use Output_Lists;

   type State_Variables (Gate : Gates) is record
      Output_List : Output_Lists.List := Output_Lists.Empty_List;
      case Gate is
      when Flip_Flop =>
         FF_State : Pulses := False; -- Initial value
      when Conjunction =>
         Input_Map : Input_Maps.Map := Input_Maps.Empty_Map;
      when Output =>
         Received : Pulses := High;
      when others =>
         Null;
      end case; -- Gates
   end record; -- State_Variables;

   package State_Variable_Maps is new
      Ada.Containers.Indefinite_Ordered_Maps (Gate_Names, State_Variables);
   use State_Variable_Maps;


   procedure Read_input (State_Variable_Map : out State_Variable_Maps.Map) is

      -- The input file is read twice first to create all the state variables
      -- and second to update the conjunction Input_Map.

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Delimiters : Character_Set := To_Set (" ->,&%");
      Gate : Gates;
      Gate_Name, Input_Name, Destination : Gate_Names;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_20.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (State_Variable_Map);
      Put_Line ("Pass 1");
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         case Element (Text, 1) is
            when '%' => Gate := Flip_Flop;
            when '&' => Gate := Conjunction;
            when others => Gate := Broadcaster;
         end case; -- Element (Text, 1)
         Start_At := 1;
         Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
         Gate_Name := Unbounded_Slice (Text, First, Last);
         Start_At := Last + 1;
         declare -- State_Variable declaration block
            State_Variable : State_Variables (Gate);
         begin
            loop -- read one ourput destination
               Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
               if Last /= 0 then
                  Append (State_Variable.Output_List,
                          Unbounded_Slice (Text, First, Last));
                  Start_At := Last + 1;
               else
                  exit;
               end if; -- Last /= 0
            end loop; -- read one ourput destination
            insert (State_Variable_Map, Gate_Name, State_Variable);
         end; -- State_Variable declaration block
      end loop; -- not End_Of_File (Input_File)
      Reset (Input_File);
      Put_Line ("Pass 2");
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
         Input_Name := Unbounded_Slice (Text, First, Last);
         Start_At := Last + 1;
         loop -- read one ourput destination
            Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
            if Last /= 0 then
               Destination := Unbounded_Slice (Text, First, Last);
               if Contains (State_Variable_Map, Destination) then
                  if State_Variable_Map (Destination).Gate = Conjunction then
                     insert (State_Variable_Map (Destination).Input_Map,
                             Input_Name, False);
                  end if; -- State_Variable_Map (Destination).Gate = Conjunction)
               else
                  declare -- State_Variable declaration block
                     State_Variable : State_Variables (Output);
                  begin
                     Include (State_Variable_Map, Destination,State_Variable);
                  end; -- State_Variable declaration block
               end if; -- Contains (State_Variable_Map, Destination)
               Start_At := Last + 1;
            else
               exit;
            end if; -- Last /= 0
         end loop; -- read one ourput destination
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   procedure Push_Button (State_Variable_Map : in out State_Variable_Maps.Map;
                         Count_High, Count_Low : in out Natural) is

      type Messages is record
         Source, Destination : Gate_Names;
         Pulse : Pulses;
      end record; -- Messages

      package Q_Interface is new
        Ada.Containers.Synchronized_Queue_Interfaces (Messages);
      use Q_Interface;

      package Message_Queues is new
        Ada.Containers.Unbounded_Synchronized_Queues (Q_Interface);
      use Message_Queues;

      Message_Queue : Message_Queues.Queue;

      procedure Send (Source, Destination : in Gate_Names;
                      Pulse : in Pulses) is

      begin -- Send
         Message_Queue.Enqueue ((Source, Destination, Pulse));
      end Send;

      Current_Message : Messages;
      Anded : Pulses;

   begin -- Push_Button
      Send (To_Unbounded_String ("button"), To_Unbounded_String ("broadcaster"),
            Low);
      while Message_Queue.Current_Use > 0 loop
         Message_Queue.Dequeue (Current_Message);
         --  Put_Line (Current_Message'Img);
         if Current_Message.Pulse then
            Count_High := @ + 1;
         else
            Count_Low := @ + 1;
         end if; -- Current_Message.Pulse
         case State_Variable_Map (Current_Message.Destination).Gate is
            when Broadcaster =>
               for O in Iterate (State_Variable_Map
                 (Current_Message.Destination).Output_List) loop
                  Send (Current_Message.Destination, Element (O),
                        Current_Message.Pulse);
               end loop; -- O in Iterate (State_Variable_Map
            when Flip_Flop =>
               if not Current_Message.Pulse then
                  State_Variable_Map (Current_Message.Destination).FF_State :=
                    not State_Variable_Map (Current_Message.Destination).
                    FF_State;
                  for O in Iterate (State_Variable_Map
                    (Current_Message.Destination).Output_List) loop
                     Send (Current_Message.Destination, Element (O),
                           State_Variable_Map (Current_Message.Destination).
                             FF_State);
                  end loop; -- O in Iterate (State_Variable_Map
               end if; -- not Current_Message.Pulse
            when Conjunction =>
               Anded := High;
               State_Variable_Map (Current_Message.Destination).
                 Input_Map (Current_Message.Source) := Current_Message.Pulse;
               for I in
                 Iterate (State_Variable_Map (Current_Message.Destination).
                              Input_Map) loop
                  Anded := @ and Element (I);
               end loop; -- I in
               for O in Iterate (State_Variable_Map
                 (Current_Message.Destination).Output_List) loop
                  Send (Current_Message.Destination, Element (O), not Anded);
               end loop; -- O in Iterate (State_Variable_Map
            when Output =>
               State_Variable_Map (Current_Message.Destination).Received :=
                 @ and Current_Message.Pulse;
               -- Latches low, allows for it being set hign again before the
               -- Message_Queue empties.
         end case; -- State_Variable_Map (Current_Message.Destination).Gate
      end loop; -- Message_Queue.Current_Use > 0
   end Push_Button;

   State_Variable_Map : State_Variable_Maps.Map;
   Count_High, Count_Low, Press_Count : Natural := 0;

begin -- December_20
   Read_input (State_Variable_Map);
   for P in Positive range 1 .. 1000 loop
      Push_Button (State_Variable_Map, Count_High, Count_Low);
   end loop; -- P in Positive range 1 .. 1000
   Put_Line ("Part one:" & Natural'Image (Count_High * Count_Low));
   DJH.Execution_Time.Put_CPU_Time;
   Count_High := 0;
   Count_Low := 0;
   while State_Variable_Map (To_Unbounded_String ("rx")).Received loop
      Press_Count := @ + 1;
      Push_Button (State_Variable_Map, Count_High, Count_Low);
   end loop; -- State_Variable_Map (To_Unbounded_String ("rx")).Received
   Put_Line ("Part two:" & Press_Count'img);
   DJH.Execution_Time.Put_CPU_Time;
end December_20;
