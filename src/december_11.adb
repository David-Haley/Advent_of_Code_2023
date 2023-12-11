with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_11 is

   subtype Long_Natural is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   subtype Ordinates is Positive;

   package Ordinate_Sets is new Ada.Containers.Ordered_Sets (Ordinates);
   use Ordinate_Sets;

   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.X < Right.X or (Left.X = Right.x and Left.Y < Right.Y));

   package Galaxy_Stores is new
     Ada.Containers.Ordered_Maps (Coordinates, Coordinates);
   -- the key is the origional coordinates and the element is the new
   -- coordinates.
   use Galaxy_Stores;

   procedure Read_input (Galaxy_Store : out Galaxy_Stores.Map;
                         X_Low, X_High, Y_Low, Y_High : out Ordinates) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Coordinate : Coordinates;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_11.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Galaxy_Store);
      X_Low := Ordinates'Last;
      X_High := Ordinates'First;
      Y_Low := Ordinates'Last;
      Y_High := Ordinates'First;
      while not End_Of_File (Input_File) loop
         Coordinate.Y := Ordinates (Line (Input_File));
         Get_Line (Input_File, Text);
         for X in Positive range 1 .. Length (Text) loop
            if Element (Text, X) = '#' then
               Coordinate.X := X;
               if X_Low > X then
                  X_Low := X;
               end if; -- X_Low > X
               if X_High < X then
                  X_High := X;
               end if; -- X_High < X
               if Y_Low > Coordinate.Y then
                  Y_Low :=Coordinate.Y;
               end if; -- Y_Low > Coordinate.Y
               if Y_High < Coordinate.Y then
                  Y_High := Coordinate.Y;
               end if; -- Y_High < Coordinate.Y
               include (Galaxy_Store, Coordinate, Coordinate);
            end if; -- Element (Text, X) = '#'
         end loop; -- X in Positive range 1 .. Length (Text)
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   procedure Find_Empty (Galaxy_Store : in Galaxy_Stores.Map;
                         X_Low, X_High, Y_Low, Y_High : in Ordinates;
                         Empty_X, Empty_Y : out Ordinate_Sets.Set) is

   begin -- Find_Empty
      Clear (Empty_X);
      Clear (Empty_Y);
      for X in Ordinates range X_Low .. X_High loop
         Include (Empty_X, X);
      end loop; -- X in Ordinates range X_Low .. X_High
      for Y in Ordinates range Y_Low .. Y_High loop
         Include (Empty_Y, Y);
      end loop; -- Y in Ordinates range Y_Low .. Y_High
      for G in Iterate (Galaxy_Store) loop
         Exclude (Empty_X, Key (G).X);
         Exclude (Empty_Y, Key (G).Y);
      end loop; -- G in Iterate (Galaxy_Store)
   end Find_Empty;

   procedure Expand (Galaxy_Store : in out Galaxy_Stores.Map;
                     Empty_X, Empty_Y : in Ordinate_Sets.Set;
                     Part_2 : Boolean := False) is

      Expansion : Ordinates;

   begin -- Expand
      if Part_2 then
         Expansion := 999999;
         -- The trick here is we are replacing one row with 1000000 there is
         -- already one row so we must add 1000000 - 1!
      else
         Expansion := 1;
      end if; -- Part_2
      for G in Iterate (Galaxy_Store) loop
         for Ex in Iterate (Empty_X) loop
            if Key (G).X > Element (Ex) then
               Galaxy_Store (G).X := Galaxy_Store (G).X + Expansion;
            end if; -- Key (G).X > Element (Ex)
         end loop; -- Ex in Iterate (Empty_X)
         for Ey in Iterate (Empty_Y) loop
            if Key (G).Y > Element (Ey) then
               Galaxy_Store (G).Y := Galaxy_Store (G).Y + Expansion;
            end if; -- Key (G).Y > Element (Ey)
         end loop; -- Ey in Iterate (Empty_Y)
      end loop; -- G in Iterate (Galaxy_Store)
   end Expand;

   function Sum_Distance (Galaxy_Store : in Galaxy_Stores.Map)
                          return Long_Natural is

      function Distance (G1, G2 : Coordinates) return Long_Natural is
        (Long_Natural (abs (G2.X - G1.X) + abs (G2.Y - G1.Y)));

      Sum : Long_Natural := 0;
      G2 : Galaxy_Stores.Cursor;

   begin -- Sum_Distance
      for G1 in iterate (Galaxy_Store) loop
         G2 := Next (G1);
         while G2 /= Galaxy_Stores.No_Element loop
            Sum := Sum + Distance (Element (G1), Element (G2));
            Next (G2);
         end loop; -- G2 /= Galaxy_Stores.No_Element
      end loop; -- G1 in iterate (Galaxy_Store)
      return Sum;
   end Sum_Distance;

   Galaxy_Store : Galaxy_Stores.Map;
   X_Low, X_High, Y_Low, Y_High : Ordinates;
   Empty_X, Empty_Y : Ordinate_Sets.Set;

begin -- December_11
   Read_input (Galaxy_Store, X_Low, X_High, Y_Low, Y_High);
   Find_Empty (Galaxy_Store, X_Low, X_High, Y_Low, Y_High, Empty_X, Empty_Y);
   Expand (Galaxy_Store, Empty_X, Empty_Y);
   Put_Line ("Part one:" & Sum_Distance (Galaxy_Store)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Read_input (Galaxy_Store, X_Low, X_High, Y_Low, Y_High);
   Find_Empty (Galaxy_Store, X_Low, X_High, Y_Low, Y_High, Empty_X, Empty_Y);
   Expand (Galaxy_Store, Empty_X, Empty_Y, True);
   Put_Line ("Part two:" & Sum_Distance (Galaxy_Store)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_11;
