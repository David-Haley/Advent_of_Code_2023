with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Interfaces; use Interfaces;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_15 is

   package Code_Stores is new
     Ada.Containers.Doubly_Linked_Lists (Unbounded_String);
   use Code_Stores;

   subtype Focal_Lengths is Positive range 1 .. 9;

   type Lenses is record
      Label : Unbounded_String;
      Focal_Length : Focal_Lengths;
   end record;

   package Boxes is new Ada.Containers.Doubly_Linked_Lists (Lenses);
   use Boxes;

   subtype Box_Numbers is Natural range 0 .. Natural (Unsigned_8'Last);

   package Box_Stores is new
     Ada.Containers.Ordered_Maps (Box_Numbers, Boxes.List);
   use Box_Stores;

   procedure Read_input (Code_Store : out Code_Stores.list) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Comma_Set : constant Character_Set := To_Set (",");

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_15.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Get_Line (Input_File, Text);
      Clear (Code_Store);
      Start_At := 1;
      loop -- read one data item
         Find_Token (Text, Comma_Set, Start_At, outside, First, Last);
         Append (Code_Store, Unbounded_Slice (Text, First, Last));
         Start_At := Last + 1;
         exit when Last = 0  or Start_At > Length (Text);
      end loop; -- read one data item
      Close (Input_File);
   end Read_input;

   function Hash (Code : in Unbounded_String) return Natural is

      Result : Unsigned_8 := 0;

   begin -- Hash
      for C in Positive range 1 .. Length (Code) loop
         Result := @ + Character'Pos (Element (Code, C));
         Result := @ * 17;
      end loop; -- C in Positive range 1 .. Length (Code)
      return Natural (Result);
   end Hash;

   procedure Build (Code_Store : in Code_Stores.List;
                    Box_Store : out Box_Stores.Map) is

      Operator_Set : Character_Set := To_Set ("-=");
      Start_At, First : Positive;
      Operator : Character;
      Last : Natural := 0;
      Lens : Lenses;
      Lc : Boxes.Cursor;
      Box_Number : Box_Numbers;
      Found : Boolean;

   begin -- Build
      Clear (Box_Store);
      for C in Iterate (Code_Store) loop
         Start_At := 1;
         Find_Token (Element (C), Operator_Set, Start_At, Outside, First, Last);
         Lens.Label := Unbounded_Slice (Element (C), First, Last);
         Box_Number := Hash (Lens.Label);
         Start_At := Last + 1;
         Operator := Element (Element (C), Start_At);
         if Operator = '=' then
            Find_Token (Element (C), Decimal_Digit_Set, Start_At, Inside, First,
                        Last);
            Lens.Focal_Length := Focal_Lengths'Value (Slice (Element (C), First,
                                                      Last));
         end if; -- Operator = '='
         case Operator is
            when '-' =>
               if Contains (Box_Store, Box_Number) then
                  Lc := Boxes.First (Box_Store (Box_Number));
                  while Lc /= Boxes.No_Element loop
                     if Element (Lc).Label = Lens.Label then
                        Delete (Box_Store (Box_Number), Lc);
                        Lc := Boxes.No_Element;
                     else
                        Next (Lc);
                     end if; -- Element (Lc).Label = Lens.Label
                  end loop; -- Lc /= Boxes.No_Element
               end if; -- Contains (Box_Store, Box_Number)
            when '=' =>
               if Contains (Box_Store, Box_Number) then
                  Lc := Boxes.First (Box_Store (Box_Number));
                  Found := False;
                  while Lc /= Boxes.No_Element loop
                     if Element (Lc).Label = Lens.Label then
                        Replace_Element (Box_Store (Box_Number), Lc, Lens);
                        Lc := Boxes.No_Element;
                        Found := True;
                     else
                        Next (Lc);
                     end if; -- Element (Lc).Label = Lens.Label
                  end loop; -- Lc /= Boxes.No_Element
                  if not Found then
                     Append (Box_Store (Box_Number), Lens);
                  end if; -- not Found
               else
                  Include (Box_Store,Box_Number, Boxes.Empty_List);
                  Append (Box_Store (Box_Number), Lens);
               end if; -- Contains (Box_Store, Box_Number)
            when others =>
               raise Program_Error with "Unknown operator in """ &
                 To_String (Element (C)) & """";
         end case; -- Operator
      end loop; -- C in Iterate (Code_Store)
   end Build;

   function Focusing_Power (Box_Store : in Box_Stores.Map) return Natural is

      Result : Natural := 0;
      Slot_Number : Positive;

   begin -- Focusing_Power
      for B in Iterate (Box_Store) loop
         Slot_Number := 1;
         for L in Iterate (Element (B)) loop
            Result := @ + (Key (B) + 1) * Slot_Number *
              Element (L).Focal_Length;
            Slot_Number := @ + 1;
         end loop; -- L in Iterate (Element (B))
      end loop; -- B in Iterate (Box_Store)
      return Result;
   end Focusing_Power;

   Code_Store : Code_Stores.List;
   Box_Store : Box_Stores.Map;
   Sum : Natural := 0;

begin -- December_15
   Read_input (Code_Store);
   for C in Iterate (Code_Store) loop
      Sum := @ + Hash (Element (C));
   end loop; -- C in Iterate (Code_Store)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Build (Code_Store, Box_Store);
   Put_Line ("Part two:" & Focusing_Power (Box_Store)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_15;
