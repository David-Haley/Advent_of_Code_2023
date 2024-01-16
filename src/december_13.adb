with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Doubly_Linked_Lists;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_13 is

   -- Base assumption is that there are only two types of terrain '.' ash and
   -- '#' rock, only the rock positions will be stored;

   subtype Ordinates is Positive;

   package Lines is new Ada.Containers.Ordered_Sets (Ordinates);
   use Lines;

   package Vectors is new Ada.Containers.Ordered_Maps (Ordinates, Lines.Set);
   use Vectors;

   type Patches is record
      Column, Row : Vectors.Map;
   end record; -- Patches

   package Patch_Stores is new Ada.Containers.Doubly_Linked_Lists (Patches);
   use Patch_Stores;

   package Mirror_Lists is new Ada.Containers.Doubly_Linked_Lists (Ordinates);

   type Smudges is record
      Object, Reflection : Ordinates;
   end record; -- Smudges

   package Smudge_Lists is new Doubly_Linked_Lists (Smudges);

   procedure Read_input (Patch_Store : out Patch_Stores.List) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Y : Ordinates;
      Patch : Patches;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_13.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Patch_Store);
      while not End_Of_File (Input_File) loop
         Clear (Patch.Column);
         Clear (Patch.Row);
         Get_Line (Input_File, Text);
         Y := 1;
         While Length (Text) > 0 and not End_Of_File (Input_File) loop
            for X in Ordinates range Ordinates'First .. Length (Text) loop
               if Element (Text, X) = '#' then
                  if not Contains (Patch.Column, X) then
                     Include (Patch.Column, X, Lines.Empty_Set);
                  end if; -- not Contains (Patch.Column, X)
                  Include (Patch.Column (X), Y);
                  if not Contains (Patch.Row, Y) then
                     Include (Patch.Row, Y, Lines.Empty_Set);
                  end if; -- not Contains (Patch.Column, X)
                  Include (Patch.Row (Y), X);
               end if; -- Elwment (Text, X) = '#'
            end loop; -- X in Ordinates range Ordinates'First .. Length (Text)
            Y := @ + 1;
            Get_Line (Input_File, Text);
         end loop; --  Length (Text) > 0 and not End_Of_File (Input_File)
         Append (Patch_Store, Patch);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   procedure Find_Mirror (Vector : in Vectors.Map;
                          Found : out Boolean;
                          Position : out Ordinates) is
      use Mirror_Lists;
      Vn : Vectors.Cursor;
      Mirror_List : Mirror_Lists.List := Mirror_Lists.Empty_List;
      Mc : Mirror_Lists.Cursor;
      O, R : Ordinates;

   begin -- Find_Mirror
      Position := 1;
      for V in Iterate (Vector) loop
         Vn := Next (V);
         if Vn /= Vectors.No_Element and then Key (V) + 1 = Key (Vn) and then
           Element (V) = Element (Vn) then
            -- pot(ential mirror found add to list
            Append (Mirror_List, Key(V));
         end if; -- Vn /= Vectors.No_Element and then Key (V) + 1 = Key (Vn) ...
      end loop; -- V in Iterate (Vector)
      Mc := First (Mirror_List);
      Found := False;
      while Mc /= Mirror_Lists.No_Element and not Found loop
         Found := True;
         Position := Element (Mc);
         O := Position;
         R := Position + 1;
         loop -- Test reflection
            If Contains (Vector, O) and Contains (Vector, R) then
               Found := @ and Element (Vector, O) = Element (Vector, R);
            else
               exit;
            end if; -- Contains (Vector, O) and Contains (Vector, R)
            exit when O = 1;
            O := @ - 1;
            R := @ + 1;
         end loop; -- Test reflection
         Next (Mc);
      end loop; -- Mc /= Mirror_Lists.No_Element and not Found
   end Find_Mirror;

   procedure  Find_Mirror_2 (Vector_In : in Vectors.Map;
                             Found : out Boolean;
                             Position : out Ordinates) is

      use Smudge_Lists;
      Vector : Vectors.Map;
      Vn : Vectors.Cursor;
      Smudge_List : Smudge_Lists.List := Smudge_Lists.Empty_List;
      Sc : Smudge_Lists.Cursor;
      Position_O, Position_R, Part_1_Position : Ordinates;
      Found_O, Found_R, Part_1_Found : Boolean;

   begin -- Find_Mirror_2
      Vector := Copy (Vector_In);
      for V in Iterate (Vector) loop
         Vn := Next (V);
         while Vn /= Vectors.No_Element loop
            if Length (Symmetric_Difference (Element (V), Element (Vn))) = 1
            then
               Append (Smudge_List, (Key (V), Key (Vn)));
            end if; --  Length (Symmetric_Difference (Element (V) ...
            Next (Vn);
         end loop; -- Vn /= Vectors.No_Element
      end loop; -- V in Iterate (Vector)
      Sc := First (Smudge_List);
      Put_Line ("Smudge_List: " & Smudge_List'Img);
      Find_Mirror (Vector, Part_1_Found, Part_1_Position);
      Put_Line ("Part_1: " & Part_1_Found'Img & Part_1_Position'Img);
      Found := False;
      while Sc /= Smudge_Lists.No_Element and not Found loop
         Vector := Copy (Vector_In);
         Put (Element (Sc)'Img);
         Vector (Element (Sc).Object) := Vector (Element (Sc).Reflection);
         Find_Mirror (Vector, Found_O, Position_O);
         if Part_1_Found and then Position_O = Part_1_Position then
            Found_O := False;
         end if; -- Part_1_Found and then Position_O = Part_1_Position
         Put (Found_O'Img & Position'Img);
         Vector := Copy (Vector_In);
         Vector (Element (Sc).Reflection) := Vector (Element (Sc).Object);
         Find_Mirror (Vector, Found_R, Position_R);
         if Part_1_Found and then Position_R = Part_1_Position then
            Found_R := False;
         end if; -- Part_1_Found and then Position_R = Part_1_Position
         Found := Found_O or Found_R;
         Put_Line (Found_R'Img & Position'Img);
         Next (Sc);
      end loop; -- Sc /= Smudge_Lists.No_Element and not Found
      if Found_O then
         Position := Position_O;
      elsif Found_R then
         Position := Position_R;
      end if; -- Found_O
   end Find_Mirror_2;

   Patch_Store : Patch_Stores.List;
   Found : Boolean;
   Position : Ordinates;
   Sum : Natural;
   Cleaned_Patch : Patches;

begin -- December_13
   Read_input (Patch_Store);
   Sum := 0;
   for P in Iterate (Patch_Store) loop
      Find_Mirror (Patch_Store (P).Row, Found, Position);
      if Found then
         Sum := @ + 100 * Position;
      end if; -- Found
      Find_Mirror (Patch_Store (P).Column, Found, Position);
      if Found then
         Sum := @ + Position;
      end if; -- Found
   end loop; -- P in Iterate (Patch_Store)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Sum := 0;
   for P in Iterate (Patch_Store) loop
      Find_Mirror_2 (Patch_Store (P).Row, Found, Position);
      if Found then
         Sum := @ + 100 * Position;
      end if; -- Found
      Find_Mirror_2 (Patch_Store (P).Column, Found, Position);
      if Found then
         Sum := @ + Position;
      end if; -- Found
   end loop; -- P in Iterate (Patch_Store)
   Put_Line ("Part two:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_13;
