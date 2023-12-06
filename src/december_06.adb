with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Numerics.Generic_Elementary_Functions;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_06 is

   subtype Long_Positive is Long_Long_Integer range 1 .. Long_Long_Integer'Last;

   type Race_Records is record
      Time, Distance : Long_Positive := 1;
   end record; -- Race_Records

   package Record_Stores is new Ada.Containers.Vectors (Positive, Race_Records);
   use Record_Stores;

   type My_Reals is digits 15;

   package Numerics is new Ada.Numerics.Generic_Elementary_Functions (My_Reals);
   use Numerics;

   procedure Read_input (Record_Store : out Record_Stores.Vector) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Race_Record : Race_Records;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_06.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Record_Store);
      Get_Line (Input_File, Text);
      Start_At := 1;
      loop -- read one time
         Find_Token (Text, Decimal_Digit_Set, Start_At, inside, First, Last);
         exit when Last = 0;
         Start_At := Last + 1;
         Race_Record.Time := Long_Positive'Value (Slice (Text, First, Last));
         Append (Record_Store, Race_Record);
      end loop; -- read one time
      Get_Line (Input_File, Text);
      Start_At := 1;
      for R in Iterate (Record_Store) loop
         Find_Token (Text, Decimal_Digit_Set, Start_At, inside, First, Last);
         Record_Store (R).Distance := Long_Positive'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
      end loop; -- R in Iterate(Record_Store)
      Close (Input_File);
   end Read_input;

   function Record_Beaters (Race_Record : in Race_Records)
                            return Long_Positive is

      Upper_Bound, Lower_Bound : Long_Positive;
      a : constant My_Reals := 1.0;
      b : constant My_Reals := - My_Reals (Race_Record.Time);
      c : constant My_Reals := My_Reals (Race_Record.Distance + 1);
      -- a result greater than the previous record is required

   begin
      Lower_Bound :=
        Long_Positive (My_Reals'Ceiling((-b - Sqrt (b ** 2 - 4.0 * a * c))
                  / (2.0 * a)));
      Upper_Bound :=
        Long_Positive (My_Reals'Floor((-b + Sqrt (b ** 2 - 4.0 * a * c))
                  / (2.0 * a)));
      return Upper_Bound - Lower_Bound + 1;
   end;

   function Concatinate (Record_Store : in Record_Stores.Vector)
                         return Race_Records is

      Race_Record : Race_Records;
      Time_String, Distance_String : Unbounded_String := Null_Unbounded_String;

   begin -- Concatinate
      for R in Iterate (Record_Store) loop
         Time_String := Time_String &
           Trim (To_Unbounded_String (Element (R).Time'Img), Both);
         Distance_String := Distance_String &
           Trim (To_Unbounded_String (Element (R).Distance'Img), Both);
      end loop;
      return (Long_Positive'Value (To_String (Time_String)),
              Long_Positive'Value (To_String (Distance_String)));
   end Concatinate;

   Record_Store : Record_Stores.Vector;
   Product : Long_Positive := 1;

begin -- December_06
   Read_input (Record_Store);
   for R in Iterate (Record_Store) loop
      Product := Product * Record_Beaters (Element (R));
   end loop; -- R in Iterate (Record_Store)
   Put_Line ("Part one:" & Product'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" & Record_Beaters (Concatinate (Record_Store))'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_06;
