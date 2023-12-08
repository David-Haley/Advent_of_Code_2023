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

   package Path_Lengths is new
     Ada.Containers.Ordered_Maps (Node_Names, Positive);
   use Path_Lengths;

   subtype Long_Positive is Long_Long_Integer range 1 .. Long_Long_Integer'Last;

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
                   Network : in Networks.Map;
                   Start : in Node_Names) return Positive is

      Steps : Natural := 0;
      subtype Instruction_Indices is Positive range 1 ..
        Length (Instruction);
      Next_Instruction : Instruction_Indices := Instruction_Indices'First;
      Next_Node : Node_Names := Start;

   begin -- Steps
      while Next_Node (3) /= 'Z' loop
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
                     Network : in Networks.Map) return Long_Positive is

      function LCM (Path_Length : in Path_Lengths.Map) return Long_Positive is

         subtype Table_Indces is Positive range
           1 .. Positive (Length (Path_Length));
         type Table_Element is record
            X, Xm : Long_Positive;
         end record; -- Table_Element
         type Tables is array (Table_Indces) of Table_Element;
         Table : Tables;
         Least, Result : Long_Positive;
         Least_Index : Table_Indces;
         All_Equal : Boolean;
         I : Positive := 1;

      begin -- LCM
         for P in Iterate (Path_Length) loop
            Table (I).X := Long_Positive (Element (P));
            Table (I).Xm := Long_Positive (Element (P));
            I := I + 1;
         end loop; -- P in Iterate (Path_Length)
         loop -- Step LCM
            All_Equal := True;
            Least := Long_Positive'Last;
            Result := Table (Table_Indces'First).Xm;
            for T in Table_Indces loop
               if Least > Table (T).Xm then
                  Least := Table (T).Xm;
                  Least_Index := T;
               end if; -- Least > Table (T).Xm
               All_Equal := All_Equal and Result = Table (T).Xm;
            end loop; -- T in Table_Indces
            exit when All_Equal;
            Table (Least_Index).Xm :=
              Table (Least_Index).Xm + Table (Least_Index).X;
         end loop; -- Step LCM
         return Result;
      end LCM;

      Path_Length : Path_Lengths.Map;

   begin -- Steps_2
      Path_Lengths.Clear (Path_Length);
      for N in Iterate (Network) loop
         if Key (N) (3) = 'A' then
            include (Path_Length, Key (N),
                     Steps (Instruction, Network, Key (N)));
         end if; -- Key (N) (3) = 'A'
      end loop; -- N in Iterate (Network)
      Return LCM (Path_Length);
   end Steps_2;

   Instruction : Unbounded_String;
   Network : Networks.Map;

begin -- December_08
   Read_input (Instruction, Network);
   Put_Line ("Part one:" & Steps (Instruction, Network, "AAA")'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" & Steps_2 (Instruction, Network)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_08;
