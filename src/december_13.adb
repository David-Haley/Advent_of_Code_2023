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
      -- two copies of the patch data are storred to allow efficient checking
      -- for the existance of vertical or horizontal mirrors.
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
         -- Note all rows and columns must be stored even if they don't comtain
         -- rocks. Empty sets need to be considered in reflections.
         While Length (Text) > 0 and not End_Of_File (Input_File) loop
            if not Contains (Patch.Row, Y) then
               Include (Patch.Row, Y, Lines.Empty_Set);
            end if; -- not Contains (Patch.Column, X)
            for X in Ordinates range Ordinates'First .. Length (Text) loop
               if not Contains (Patch.Column, X) then
                  Include (Patch.Column, X, Lines.Empty_Set);
               end if; -- not Contains (Patch.Column, X)
               if Element (Text, X) = '#' then
                  Include (Patch.Column (X), Y);
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
                          Mirror_Set : out Lines.Set) is

      -- Finds the set of mirrors by intiially locking at adjacent pairs of
      -- lines that match and then checking that lines working away also match.

      use Mirror_Lists;
      Vn : Vectors.Cursor;
      Mirror_List : Mirror_Lists.List := Mirror_Lists.Empty_List;
      Mc : Mirror_Lists.Cursor;
      O, R : Ordinates;
      Found : Boolean;

   begin -- Find_Mirror
      Clear (Mirror_Set);
      -- Find matching pairs
      for V in Iterate (Vector) loop
         Vn := Next (V);
         if Vn /= Vectors.No_Element and then Key (V) + 1 = Key (Vn) and then
           Element (V) = Element (Vn) then
            -- pot(ential mirror found add to list
            Append (Mirror_List, Key(V));
         end if; -- Vn /= Vectors.No_Element and then Key (V) + 1 = Key (Vn) ...
      end loop; -- V in Iterate (Vector)
      -- Check that this is truly a mirror by comparing the rest of the patch.
      Mc := First (Mirror_List);
      while Mc /= Mirror_Lists.No_Element loop
         Found := True;
         O := Element (Mc);
         R := Element (Mc) + 1;
         loop -- Test reflection
            Found := @ and Element (Vector, O) = Element (Vector, R);
            exit when O = Ordinates'First or R = Last_Key (Vector);
            O := @ - 1;
            R := @ + 1;
         end loop; -- Test reflection
         if Found then
            -- Store the location of the mirror, can be more than 1, e.g. second
            -- patch in example in part 2.
            Include (Mirror_Set, Element (Mc));
         end if; -- Found
         Next (Mc);
      end loop; -- Mc /= Mirror_Lists.No_Element
   end Find_Mirror;

   procedure  Find_Mirror_2 (Vector_In : in Vectors.Map;
                             Mirror_Set : out Lines.Set) is

      -- Find pairs of lines with one difference. Then check that this produces
      -- one or more mirrors that are different from the part one mirror.

      use Smudge_Lists;
      Vector : Vectors.Map;
      Vn : Vectors.Cursor;
      Smudge_List : Smudge_Lists.List := Smudge_Lists.Empty_List;
      Sc : Smudge_Lists.Cursor;
      Mirror_Set_O, Mirror_Set_R, Part_1_Mirror_Set : Lines.Set;

   begin -- Find_Mirror_2
      Clear (Mirror_Set);
      -- Find potential smudges.
      for V in Iterate (Vector_In) loop
         Vn := Next (V);
         while Vn /= Vectors.No_Element loop
            if Length (Symmetric_Difference (Element (V), Element (Vn))) = 1
            then
               Append (Smudge_List, (Key (V), Key (Vn)));
            end if; --  Length (Symmetric_Difference (Element (V) ...
            Next (Vn);
         end loop; -- Vn /= Vectors.No_Element
      end loop; -- V in Iterate (Vector_In)
      -- Find mirrors resulting from correction of the smudge. Both options of
      -- removing a rock and inserting a rock are seperatly tested.
      Sc := First (Smudge_List);
      while Sc /= Smudge_Lists.No_Element loop
         Vector := Copy (Vector_In);
         Vector (Element (Sc).Object) := Vector (Element (Sc).Reflection);
         Find_Mirror (Vector, Mirror_Set_O);
         Mirror_Set := Union (Mirror_Set, Mirror_Set_O);
         Vector := Copy (Vector_In);
         Vector (Element (Sc).Reflection) := Vector (Element (Sc).Object);
         Find_Mirror (Vector, Mirror_Set_R);
         Mirror_Set := Union (Mirror_Set, Mirror_Set_R);
         Next (Sc);
      end loop; -- Sc /= Smudge_Lists.No_Element
      -- Remove the mirror if it is the same as part 1
      Find_Mirror (Vector_In, Part_1_Mirror_Set);
      if Length (Part_1_Mirror_Set) = 1 then
         Exclude (Mirror_Set, First_Element (Part_1_Mirror_Set));
      end if; -- Length (Part_1_Mirror_Set) = 1
   end Find_Mirror_2;

   Patch_Store : Patch_Stores.List;
   Mirror_Set : Lines.Set;
   Sum : Natural;

begin -- December_13
   Read_input (Patch_Store);
   Sum := 0;
   for P in Iterate (Patch_Store) loop
      Find_Mirror (Patch_Store (P).Row, Mirror_Set);
      if Length (Mirror_Set) = 1 then
         Sum := @ + 100 * First_Element (Mirror_Set);
      end if; -- Length (Mirror_Set) = 1
      Find_Mirror (Patch_Store (P).Column, Mirror_Set);
      if Length (Mirror_Set) = 1 then
         Sum := @ + 1 * First_Element (Mirror_Set);
      end if; -- Length (Mirror_Set) = 1
   end loop; -- P in Iterate (Patch_Store)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Sum := 0;
   for P in Iterate (Patch_Store) loop
      Find_Mirror_2 (Patch_Store (P).Row, Mirror_Set);
      if Length (Mirror_Set) = 1 then
         Sum := @ + 100 * First_Element (Mirror_Set);
      end if; -- Length (Mirror_Set) = 1
      Find_Mirror_2 (Patch_Store (P).Column, Mirror_Set);
      if Length (Mirror_Set) = 1 then
         Sum := @ + 1 * First_Element (Mirror_Set);
      end if; -- Length (Mirror_Set) = 1
   end loop; -- P in Iterate (Patch_Store)
   Put_Line ("Part two:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_13;
