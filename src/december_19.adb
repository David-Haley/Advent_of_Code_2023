with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_19 is

   type Properties is (X, M, A, S);

   subtype Values is Long_Long_Integer range 1 .. 4000;
   -- Upper bound Known after part 1 complete

   type Bounds is record
      Lower : Values := Values'First;
      Upper : Values := Values'Last;
   end record; -- Bounds;

   type Bound_Arrays is array (Properties) of Bounds;

   package Bound_Stores is new
     Ada.Containers.Doubly_Linked_Lists (Bound_Arrays);
   use Bound_Stores;

   type Parts is array (Properties) of Values;

   package Part_Stores is new Ada.Containers.Doubly_Linked_Lists (Parts);
   use Part_Stores;

   subtype Flow_Names is Unbounded_String;

   package Rule_Lists is new Ada.Containers.Doubly_Linked_Lists (Flow_Names);
   use Rule_Lists;

   subtype Operators is Character with
     Static_Predicate => Operators in '<' | '>';

   subtype Outcomes is  Character with
     Static_Predicate => Outcomes in 'A' | 'R';

   subtype Actions is Unbounded_String;

   package Workflow_Stores is new
     Ada.Containers.Ordered_Maps (Flow_Names, Actions);
   use Workflow_Stores;

   procedure Read_input (Workflow_Store : out Workflow_Stores.Map;
                         Part_Store : out Part_Stores.List) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Braces_Set : constant Character_Set := To_Set ("{}");
      Flow_Name : Flow_Names;
      Action : Actions;
      Part : Parts;
      Property : Properties;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_19.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Workflow_Store);
      Clear (Part_Store);
      Get_Line (Input_File, Text);
      while Length (Text) > 0 loop
         Start_At := 1;
         Find_Token (Text, Braces_Set, Start_At, Outside, First, Last);
         Flow_Name := Unbounded_Slice (Text, First, Last);
         Start_At := Last + 1;
         Find_Token (Text, Braces_Set, Start_At, Outside, First, Last);
         Action := Unbounded_Slice (Text, First, Last);
         Insert (Workflow_Store, Flow_Name, Action);
         Get_Line (Input_File, Text);
      end loop; -- Length (Text) > 0
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         for I in Properties loop
            Find_Token (Text, Lower_Set, Start_At, Inside, First, Last);
            Property := Properties'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            Part (Property) :=
              Values'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
         end loop; -- I in Properties
         append (Part_Store, Part);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   function Is_Accepted (Workflow_Store : in Workflow_Stores.Map;
                         Part : in Parts;
                         Action : in Actions) return Boolean is

      function One_Test (Action : in Actions;
                         Part : in Parts;
                         Last : out Positive) return Boolean is

         -- Side effect Last is the last character in the test

         First : Positive;
         Property : Properties := Properties'Value (Slice (Action, 1, 1));
         Operator : Operators := Element (Action, 2);
         Value : Values;

      begin -- One_Test
         Find_Token (Action, Decimal_Digit_Set, 3, Inside, First, Last);
         Value :=Values'Value (Slice (Action, First, Last));
         case Operator is
         when '<' =>
            return Part (Property) < Value;
         when '>' =>
            return Part (Property) > Value;
         end case; -- Operator
      end One_Test;

      Test_Result : Boolean;
      Start_At, First : Positive;
      Last : Natural;
      True_Action, False_Action : Actions;
      Action_Delimiters : constant Character_Set := To_Set (":,");

   begin -- Is_Accepted
      Test_Result := One_Test (Action, Part, Start_At);
      Start_At := @ + 1;
      Find_Token (Action, Action_Delimiters, Start_At, Outside, First, Last);
      True_Action := Unbounded_Slice (Action, First, Last);
      False_Action := Unbounded_Slice (Action, Last + 2, Length (Action));
      if Test_Result then
         if Element (True_Action, 1) = 'A' then
            return True;
         elsif Element (True_Action, 1) = 'R' then
            return False;
         else
            return
              Is_Accepted (Workflow_Store, Part, Workflow_Store (True_Action));
         end if; -- Element (True_Action, 1) = 'A'
      else
         if Element (False_Action, 1) = 'A' then
            return True;
         elsif  Element (False_Action, 1) = 'R' then
            return False;
         elsif Contains (Workflow_Store, False_Action) then
            return
              Is_Accepted (Workflow_Store, Part, Workflow_Store (False_Action));
         else
            return Is_Accepted (Workflow_Store, Part, False_Action);
         end if; -- Element (True_Action, 1) = 'A'
      end if; -- Test_Result
   end Is_Accepted;

   procedure Find_Bounds (Workflow_Store : in Workflow_Stores.Map;
                          Flow_Name : in Flow_Names;
                          Bound_Array_In : in Bound_Arrays;
                          Bound_Store : in out Bound_Stores.List) is

      function Get_Value (Action : in Actions;
                          Start_At : in out Positive) return Values is

         -- Has side effect of setting Start_At to character after ':'

         First : Positive;
         Last : Natural;

      begin -- Get_Value
         Find_Token (Action, Decimal_Digit_Set, Start_AT, Inside, First, Last);
         Start_At := Last + 2;
         if Length (Action) < Last + 1 or else
           Element (Action, Last + 1 ) /= ':' then
            raise Program_Error with "Test action not found";
         end if; -- Length (Action) < Last + 1 or else ...
         return Values'Value (Slice (Action, First, Last));
      end Get_Value;

      Start_At : Positive := 1;
      First : Positive;
      Last : Natural;
      Comma_Set : constant Character_Set := To_Set (",");
      True_Bound : Bound_Arrays;
      False_Bound : Bound_Arrays := Bound_Array_In;
      Action : Actions := Workflow_Store (Flow_Name);
      Property : Properties;
      Operator : Operators;
      Value : Values;

   begin -- Find_Bounds
      loop -- Process one workflow element
         Find_Token (Action, Comma_Set, Start_At, Outside, First, Last);
         Start_At := Last + 1;
         if Element (Action, First) = 'A' then
            Append (Bound_Store, False_Bound);
         elsif  Element (Action, First)  = 'R' then
            Null;
         elsif First + 1 < Last and then
           Element (Action, First + 1) in Operators then
            True_Bound := False_Bound;
            Property := Properties'Value (Slice (Action, First, First));
            First := First + 1;
            Operator := Element (Action, First);
            First := First + 1;
            Value := Get_Value (Action, First);
            case Operator is
               when '>' =>
                  if True_Bound (Property).Lower <= Value then
                     True_Bound (Property).Lower := Value + 1;
                  end if; -- True_Bound (Property).Lower <= Value
                  if False_Bound (Property).Upper > Value then
                     False_Bound (Property).Upper := Value;
                  end if; -- False_Bound (Property).Upper > Value
               when '<' =>
                  if True_Bound (Property).Upper >= Value then
                     True_Bound (Property).Upper := Value - 1;
                  end if; -- True_Bound (Property).Upper >= Value
                  if False_Bound (Property).Lower < Value then
                     False_Bound (Property).Lower := Value;
                  end if; -- False_Bound (Property).Lower < Value
            end case; -- Operator
            if Element (Action, First) = 'A' then
               Append (Bound_Store, True_Bound);
            elsif Element (Action, First) = 'R' then
               Null;
            else
               Find_Bounds (Workflow_Store,
                            Unbounded_Slice (Action, First, Last),
                            True_Bound,
                            Bound_Store);
            end if; -- Element (Action, Start_A) = 'A'
         else
            Find_Bounds (Workflow_Store,
                         Unbounded_Slice (Action, First, Last),
                         False_Bound,
                         Bound_Store);
         end if; -- Element (Action, First) = 'A'
         exit when Start_At >= Length (Action);
      end loop; -- Process one workflow element
   end Find_Bounds;

   function Sum_Bounds (Bound_Store : in Bound_Stores.List)
                        return Long_Long_Integer is

      function Get_Bound (Bound_Store : in Bound_Stores.List;
                          B : in Bound_Stores.Cursor;
                          P : In Properties)
                          return Long_Long_Integer is

      begin -- Get_Bound
         if Element (B) (P).Lower < Element (B) (P).Upper then
            return Element (B) (P).Upper - Element (B) (P).Lower + 1;
            -- Got to watch that + 1, a range of 2 to 3 has two elements!
         else
            return 0;
         end if; -- Element (B) (P).Lower < Element (B) (P).Upper
      end Get_Bound;

      Sum : Long_Long_Integer := 0;
      Product : Long_Long_Integer;

   begin -- Sum_Bounds
      for B in Iterate (Bound_Store) loop
         Product := 1;
         for P in Properties loop
            Product := @ * Get_Bound (Bound_Store, B, P);
         end loop; -- P in Properties
         Sum := @ + Product;
      end loop; -- B in Iterate (Bound_Store)
      return Sum;
   end Sum_Bounds;

   Workflow_Store : Workflow_Stores.Map;
   Part_Store : Part_Stores.List;
   Sum : Long_Long_Integer;
   Starting_Bounds : Bound_Arrays; -- initialised by record declaration
   Bound_Store : Bound_Stores.List := Bound_Stores.Empty_List;
   Workflow_Start : constant Unbounded_String := To_Unbounded_String ("in");

begin -- December_19
   Read_input (Workflow_Store, Part_Store);
   Sum := 0;
   for Part in Iterate (Part_Store) loop
      if Is_Accepted (Workflow_Store, Element (Part),
                      Workflow_Store (Workflow_Start)) then
         for Prperty in Properties loop
            Sum := @ + Element (Part) (Prperty);
         end loop; -- Prperty in Properties
      end if; --  Is_Accepted (Workflow_Store, Element (Part), ...
   end loop; -- P in Iterate (Part_Store)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Find_Bounds (Workflow_Store, Workflow_Start, Starting_Bounds, Bound_Store);
   Put_Line ("Part two:" & Sum_Bounds (Bound_Store)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_19;
