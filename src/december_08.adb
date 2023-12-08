with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_08 is

   subtype Node_Names is String (1 .. 3);

   type Nodes is record
      Left, Right : Node_Names;
   end record; -- Nodes;

   package Networks is new Ada.Containers.Ordered_Maps (Node_Names, Nodes);
   use Networks;

   package Node_Maps is new
     Ada.Containers.Ordered_Maps (Node_Names, Node_Names);
   use Node_Maps;

   procedure Read_input (Instruction : out Unbounded_String;
                         Network : out Networks.Map) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Node_Name : Node_Names;
      Node : Nodes;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_08.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Network);
      Get_Line (Input_File, Instruction);
      Skip_Line (Input_File);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Upper_Set, Start_At, inside, First, Last);
         Node_Name := Slice (Text, First, Last);
         Start_At := Last + 1;
         Find_Token (Text, Upper_Set, Start_At, inside, First, Last);
         Node.Left := Slice (Text, First, Last);
         Start_At := Last + 1;
         Find_Token (Text, Upper_Set, Start_At, inside, First, Last);
         Node.Right := Slice (Text, First, Last);
         Include (Network, Node_Name, Node);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   function Steps (Instruction : in Unbounded_String;
                   Network : in Networks.Map) return Positive is

      Steps : Natural := 0;
      Next_Node : Node_Names := "AAA";
      End_Node : constant Node_Names := "ZZZ";
      subtype Instruction_Indices is Positive range 1 ..
        Length (Instruction);
      Next_Instruction : Instruction_Indices := Instruction_Indices'First;

   begin -- Steps
      while Next_Node /= End_Node loop
         case Element (Instruction, Next_Instruction) is
            when 'L' =>
               Next_Node := Network (Next_Node).Left;
            when 'R' =>
               Next_Node := Network (Next_Node).Right;
            when others =>
               raise Program_Error with "Expected 'L' or 'R' and found '" &
                 Element (Instruction, Next_Instruction) & "'";
         end case; -- Element (Instruction, Next_Instruction)
         Steps := Steps + 1;
         if Next_Instruction < Instruction_Indices'Last then
            Next_Instruction := Next_Instruction + 1;
         else
            Next_Instruction := Instruction_Indices'First;
         end if; -- Next_Instruction < Instruction_Indices'Last
      end loop; -- Next_Node /= End_Node
      return Steps;
   end Steps;

   function Steps_2 (Instruction : in Unbounded_String;
                   Network : in Networks.Map) return Positive is

      function All_Zs (Next_Node : Node_Maps.Map) return Boolean is

         Result : Boolean := True;

      begin -- All_Zs
         for N in Iterate (Next_Node) loop
            Result := Result and Next_Node (N) (3) = 'Z';
         end loop; -- N in Iterate (Next_Node)
         return Result;
      end All_Zs;

      Steps : Natural := 0;
      subtype Instruction_Indices is Positive range 1 ..
        Length (Instruction);
      Next_Instruction : Instruction_Indices := Instruction_Indices'First;
      Next_Node : Node_Maps.Map;

   begin -- Steps_2
      Clear (Next_Node);
      for N in Iterate (Network) loop
         if Key (N) (3) = 'A' then
            Include (Next_Node, Key (N), Key (N));
         end if; -- Key (N) (3) = 'A'
      end loop; -- N in Iterate (Network)

      for N in Iterate (Next_Node) loop
        Put_Line (Key (N) & ": " & Element (N));
      end loop;

      while not All_Zs (Next_Node) loop
         for N in Iterate (Next_Node) loop
            case Element (Instruction, Next_Instruction) is
            when 'L' =>
               Next_Node (N) := Network (Next_Node (N)).Left;
            when 'R' =>
               Next_Node (N) := Network (Next_Node (N)).Right;
            when others =>
               raise Program_Error with "Expected 'L' or 'R' and found '" &
                 Element (Instruction, Next_Instruction) & "'";
            end case; -- Element (Instruction, Next_Instruction)
         end loop; -- N in Iterate (Next_Node);
         Steps := Steps + 1;
         if Next_Instruction < Instruction_Indices'Last then
            Next_Instruction := Next_Instruction + 1;
         else
            Next_Instruction := Instruction_Indices'First;
         end if; -- Next_Instruction < Instruction_Indices'Last
      end loop; -- Next_Node /= End_Node
      return Steps;
   end Steps_2;

   Instruction : Unbounded_String;
   Network : Networks.Map;

begin -- December_08
   Read_input (Instruction, Network);
   --  Put_Line ("Part one:" & Steps (Instruction, Network)'Img);
   --  DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" & Steps_2 (Instruction, Network)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_08;
