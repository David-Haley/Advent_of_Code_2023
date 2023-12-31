with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_22 is

   subtype Ordinates is Natural;

   type Points is record
      X, Y, Z : Ordinates;
   end record; -- Points

   type Bricks is record
      P1, P2 : Points;
   end record; -- Bricks

   subtype Brick_IDs is Positive;

   package Brick_Stores is new Ada.Containers.Vectors (Brick_IDs, Bricks);
   use Brick_Stores;

   function "<" (Left, Right : Bricks) return Boolean is
     (Left.P1.Z < Right.P1.Z);

   package Brick_Sort is new Brick_Stores.Generic_Sorting;

   package Support_Sets is new Ada.Containers.Ordered_Sets (Brick_IDs);
   use Support_Sets;

   type Dropped_Bricks is record
      P1, P2 : Points;
      Is_Top : Boolean := True;
      Brick_ID : Brick_IDs;
      Support_Set : Support_Sets.Set := Support_Sets.Empty_Set;
   end record; -- Dropped_Bricks

   package Dropped_Brick_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Dropped_Bricks);
   use Dropped_Brick_Lists;

   package Brick_Stacks is new
     Ada.Containers.Ordered_Maps (Ordinates, Dropped_Brick_Lists.List);
   use Brick_Stacks;
   -- The Z coordinate is the Key and the list is bricks with their top element
   -- in that plane.

   procedure Read_Input (Brick_Store : out Brick_Stores.Vector) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Brick : Bricks;
      Start_At, First : Positive;
      Last : Natural;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_22.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Brick_Store);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Brick.P1.X := Ordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Brick.P1.Y := Ordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Brick.P1.Z := Ordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Brick.P2.X := Ordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Brick.P2.Y := Ordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Brick.P2.Z := Ordinates'Value (Slice (Text, First, Last));
         Append (Brick_Store, Brick);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   procedure Drop (Brick : in Bricks;
                   Brick_ID : in Brick_IDs;
                   Brick_Stack : in out Brick_Stacks.Map) is

      function Collide (Brick : in Bricks;
                        Dropped_Brick : in Dropped_Bricks) return Boolean is

         Result : Boolean;

      begin -- Collide
         if Dropped_Brick.P1.X = Dropped_Brick.P2.X then
            if Brick.P1.X = Brick.P2.X then
               -- Parallel, colinear or overlapping
               Result := Dropped_Brick.P1.X = Brick.P1.X and then
                 ((Dropped_Brick.P1.Y <= Brick.P1.Y and
                       Brick.P1.Y <= Dropped_Brick.P2.Y) or
                    (Dropped_Brick.P1.Y <= Brick.P2.Y and
                         Brick.P2.Y <= Dropped_Brick.P2.Y) or
                    (Brick.P1.Y <= Dropped_Brick.P1.Y and
                         Dropped_Brick.P1.Y <= Brick.P2.Y) or
                    (Brick.P1.Y <= Dropped_Brick.P2.Y and
                         Dropped_Brick.P2.Y <= Brick.P2.Y));
            else
               -- Brick.P1.Y = Brick.P2.Y => Perpendicular
               Result := (Dropped_Brick.P1.Y <= Brick.P1.Y and
                            Brick.P1.Y <= Dropped_Brick.P2.Y) and then
                 (Brick.P1.X <= Dropped_Brick.P1.X and
                    Dropped_Brick.P1.X <= Brick.P2.X);
            end if; -- Brick.P1.X = Brick.P2.X
         else
            -- Dropped_Brick.P1.Y = Dropped_Brick.P2.Y
            if Brick.P1.Y = Brick.P2.Y then
               -- Parallel, colinear or overlapping
               Result := Dropped_Brick.P1.Y = Brick.P1.Y and then
                 ((Dropped_Brick.P1.X <= Brick.P1.X and
                       Brick.P1.X <= Dropped_Brick.P2.X) or
                    (Dropped_Brick.P1.X <= Brick.P2.X and
                         Brick.P2.X <= Dropped_Brick.P2.X) or
                    (Brick.P1.X <= Dropped_Brick.P1.X and
                         Dropped_Brick.P1.X <= Brick.P2.X) or
                    (Brick.P1.X <= Dropped_Brick.P2.X and
                         Dropped_Brick.P2.X <= Brick.P2.X));
            else
               -- Brick.P1.X = Brick.P2.X => Perpendicular
               Result := (Brick.P1.Y <= Dropped_Brick.P1.Y and
                            Dropped_Brick.P1.Y <= Brick.P2.Y) and then
                 (Dropped_Brick.P1.X <= Brick.P1.X and
                    Brick.P1.X <= Dropped_Brick.P2.X);
            end if; -- Brick.P1.Y = Brick.P2.Y
         end if; -- Dropped_Brick.P1.X = Dropped_Brick.P2.X
         return Result;
      end Collide;

      procedure Place (Brick : in Bricks;
                       Brick_ID : in Brick_IDs;
                       Z : in Ordinates;
                       Brick_Stack : in out Brick_Stacks.Map) is

         Dropped_Brick : Dropped_Bricks;

      begin -- Place
         Dropped_Brick.P1 := Brick.P1;
         Dropped_Brick.P2 := Brick.P2;
         Dropped_Brick.P1.Z := Z;
         Dropped_Brick.P2.Z := Z - Brick.P1.Z + Brick.P2.Z;
         Dropped_Brick.Brick_ID := Brick_ID;
         if not Contains (Brick_Stack, Dropped_Brick.P2.Z) then
            Insert (Brick_Stack, Dropped_Brick.P2.Z,
                    Dropped_Brick_Lists.Empty_List);
         end if; -- not Contains (Brick_Stack, Dropped_Brick.P2.Z)
         Append (Brick_Stack (Dropped_Brick.P2.Z), Dropped_Brick);
      end Place;

      Zc : Brick_Stacks.Cursor := Last (Brick_Stack);
      Has_Collision : Boolean := False;

   begin -- Drop
      while Zc /= Brick_Stacks.No_Element and not Has_Collision loop
         for Dc in Iterate (Brick_Stack (Zc)) loop
            if Collide (Brick, Brick_Stack (Zc) (Dc)) then
               Brick_Stack (Zc) (Dc).Is_Top := False;
               Has_Collision := True;
               Insert (Brick_Stack (Zc) (Dc).Support_Set, Brick_ID);
            end if; -- Collide (Brick_Stack (Zc) (Dc), Brick)
         end loop; -- Dc in Iterate (Brick_Stack (Zc))
         if Has_Collision then
            Place (Brick, Brick_ID, Key (Zc) + 1, Brick_Stack);
         end if; -- Has_Collision
         Previous (Zc);
      end loop; -- Zc /= Brick_Stacks.No_Element and not Has_Collision
      if not Has_Collision then
         Place (Brick, Brick_ID, 1, Brick_Stack);
      end if; -- not Has_Collision
   end Drop;

   function Can_Disintergrate (Brick_Stack : in Brick_Stacks.Map)
                               return Natural is
      Count : Natural := 0;
      Support_Set : Support_Sets.Set;

   begin -- Can_Disintergrate
      for Z in Iterate (Brick_Stack) loop
         for B in Iterate (Brick_Stack (Z)) loop
            if Element (B).Is_Top then
               Count := @ + 1;
            else
               Clear (Support_Set);
               for B2 in Iterate (Brick_Stack (Z)) loop
                  if Element (B).Brick_ID /= Element (B2).Brick_ID then
                     Support_Set :=
                       Union (Support_Set, Element (B2).Support_Set);
                  end if; -- B2 in  Iterate (Brick_Stack (Z))
               end loop; -- B2 in Iterate (Brick_Stack (Z2))
               if Is_Subset (Element (B).Support_Set, Support_Set) then
                  -- To be a subset at least one other brick must support all
                  -- bricks supported by Element (B).
                  Count := @ + 1;
               end if; -- Is_Subset (Element (B).Support_Set, Support_Set)
            end if; -- Element (B).Is_Top
         end loop; -- B in Iterate (Brick_Stack (Z))
      end loop; -- Z in Iterate (Brick_Stack)
      return Count;
   end Can_Disintergrate;

   Brick_Store : Brick_Stores.Vector;
   Brick_Stack : Brick_Stacks.Map := Brick_Stacks.Empty_Map;

begin -- December_22
   Read_input (Brick_Store);
   Brick_Sort.Sort (Brick_Store);
   for B in Iterate (Brick_Store) loop
      Drop (Element (B), To_Index (B), Brick_Stack);
   end loop; -- B in Iterate (Brick_Store)
   Put_Line ("Part one:" & Can_Disintergrate (Brick_Stack)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_22;
