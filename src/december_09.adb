with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_09 is

   subtype Data is Integer;

   package Values is new
     Ada.Containers.Doubly_Linked_Lists (Data);
   use Values;

   subtype Levels is Natural;

   package Value_Maps is new
     Ada.Containers.Ordered_Maps (Levels, Values.List);
   use Value_Maps;

   package Data_Stores is new
     Ada.Containers.Vectors (Positive, Value_Maps.Map);
   use Data_Stores;

   procedure Read_input (Data_Store : out Data_Stores.Vector) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      integer_Set : constant Character_Set := To_Set ("0123456789-");

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_09.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Data_Store);
      while not End_Of_File (Input_File) loop
         Append (Data_Store, Value_Maps.Empty_Map);
         Include (Data_Store (Last_Index(Data_Store)), Levels'First,
                  Values.Empty_List);
         Get_Line (Input_File, Text);
         Start_At := 1;
         loop -- read one data item
            Find_Token (Text, integer_Set, Start_At, inside, First, Last);
            Append (Data_Store (Last_Index(Data_Store)) (Levels'First),
                    Data'Value (Slice (Text, First, Last)));
            Start_At := Last + 1;
            exit when Last = 0  or Start_At > Length (Text);
         end loop; -- read one data item
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   procedure Extrapolate (Value_Map : in out Value_Maps.Map;
                          Past, Future : out Data) is

      function All_Equal (Value : Values.List) return Boolean is

         Result : Boolean := True;

      begin -- All_Equal
         for V in Iterate (Value) loop
            Result := Result and Element (V) = First_Element (Value);
         end loop; -- V in Iterate (Value)
         return Result;
      end All_Equal;

      Level : Levels := Levels'First;
      Next_Level : Levels := Level + 1;
      Pc, Cc : Values.Cursor;

   begin -- Extrapolate
      loop -- reduce to no difference
         Pc := First (Value_Map (Level));
         Cc := Next (Pc);
         include (Value_Map, Next_Level, Values.Empty_List);
         loop -- create next level differences
            Append (Value_Map (Next_Level), Element (Cc) - Element (Pc));
            exit when Cc = Last (Value_Map (Level));
            Pc := Cc;
            Next (Cc);
         end loop; -- create next level differences
         exit when All_Equal (Value_Map (Next_Level));
         Level := Next_Level;
         Next_Level := Next_Level + 1;
      end loop; -- reduce to no difference
      Level := Last_Key (Value_Map);
      Next_Level := Level - 1;
      loop -- calculate new value for one level
         Append (Value_Map (Next_Level),
                 Last_Element (Value_Map (Next_Level)) +
                   Last_Element (Value_Map (Level)));
         Prepend (Value_Map (Next_Level),
                  First_Element (Value_Map (Next_Level)) -
                    First_Element (Value_Map (Level)));
         exit when Next_Level = Levels'First;
         Level := Next_Level;
         Next_Level := Next_Level - 1;
      end loop; -- calculate new value for one level
      Past := First_Element (Value_Map (Levels'First));
      Future := Last_Element (Value_Map (Levels'First));
   end Extrapolate;

   Data_Store : Data_Stores.Vector;
   Past, Future : Data;
   Past_Sum, Future_Sum : Data := 0;

begin -- December_09
   Read_input (Data_Store);
   for D in Iterate (Data_Store) loop
      Extrapolate (Data_Store (D), Past, Future);
      Future_Sum := Future_Sum + Future;
      Past_Sum := Past_Sum + Past;
   end loop; -- D in Iterate (Data_Store)
   Put_Line ("Part one:" & Future_Sum'Img);
   Put_Line ("Part two:" & Past_Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_09;
