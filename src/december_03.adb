with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_03 is

   subtype Coordinates is Natural;
   -- N.B. 0 is outside map, allows subtraction of 1 and remaining in Natural
   -- range. This allows inclusion tests on maps without having to test
   -- coordinate validity first.

   type Numbers is record
      X_Left, X_Right, Y : Coordinates;
      Value : Positive;
   end record; -- Numbers

   subtype Number_Indices is Positive;

   package Number_Stores is new
     Ada.Containers.Vectors (Number_Indices, Numbers);
   use Number_Stores;

   type Symbol_Keys is record
      X, Y : Coordinates;
   end record; -- Symbol_Keys

   function "<" (Left, Right : Symbol_Keys) return Boolean is
     (Left.Y < Right.Y or (Left.Y = Right.Y and Left.X < Right.X));

   package Symbol_Stores is new
     Ada.Containers.Ordered_Maps (Symbol_Keys, Character);
   use Symbol_Stores;

   package Number_Maps is new
     Ada.Containers.Ordered_Maps (Symbol_Keys, Number_Indices);
   use Number_Maps;

   package Number_Sets is new
     Ada.Containers.Ordered_Sets (Number_Indices);
   use Number_Sets;

   procedure Read_input (Number_Store : out Number_Stores.Vector;
                         Symbol_Store : out Symbol_Stores.Map) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Symbol_Key : Symbol_Keys;
      Number : Numbers;

   begin -- Read_input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_03.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Number_Store);
      Clear (Symbol_Store);
      Symbol_Key.Y := 1;
      Number.Y := 1;
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         loop -- read one number
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            if Last /= 0 then
               Number.X_Left := First;
               Number.X_Right := Last;
               Number.Value := Positive'Value (Slice (Text, First, Last));
               Append (Number_Store, Number);
               Start_At := Last + 1;
            end if;
            exit when Last = 0 or Start_At > Length (Text);
         end loop; -- read one number
         for X in Coordinates range 1 .. Length (Text) loop
            if not Is_In (Element (Text, X), Decimal_Digit_Set) and
              Element (Text, X) /= '.' then
               Symbol_Key.X := X;
               include (Symbol_Store, Symbol_Key, Element (Text, X));
            end if; -- not (Element (Text, X) in Decimal_Digit_Set)
         end loop; -- X in Coordinates range 1 .. Length (Text)
         Symbol_Key.Y := Symbol_Key.Y + 1;
         Number.Y := Number.Y + 1;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_input;

   function Is_Part_Number (Number : in Numbers;
                            Symbol_Store : in Symbol_Stores.Map)
                            return Boolean is

      Result : Boolean := False;

   begin -- Is_Part_Number
      for X in Coordinates range Number.X_Left .. Number.X_Right loop
         Result := Result or
           Contains (Symbol_Store, (X - 1, Number.Y - 1)) or
           Contains (Symbol_Store, (X - 1, Number.Y)) or
           Contains (Symbol_Store, (X - 1, Number.Y + 1)) or
           Contains (Symbol_Store, (X, Number.Y - 1)) or
           Contains (Symbol_Store, (X, Number.Y + 1)) or
           Contains (Symbol_Store, (X + 1, Number.Y - 1)) or
           Contains (Symbol_Store, (X + 1, Number.Y)) or
           Contains (Symbol_Store, (X + 1, Number.Y + 1));
      end loop; -- X in Coordinates range Number.X_Left .. Number.X_Right
      return Result;
   end Is_Part_Number;

   function Gear_Ratios (Number_Store : in Number_Stores.Vector;
                         Symbol_Store : in Symbol_Stores.Map) return Natural is

      procedure Build (Number_Store : in Number_Stores.Vector;
                       Number_Map : out Number_Maps.Map) is

      begin -- Build
         Clear (Number_Map);
         for N in Iterate (Number_Store) loop
            for X in Coordinates range
              Element (N).X_Left .. Element (N).X_Right loop
               Include (Number_Map, (X, Element (N).Y), To_Index (N));
            end loop; -- X in Coordinates range ...
         end loop; -- N in Iterate (Number_Store)
      end Build;

      procedure Conditional_Include (Number_Set : in out Number_Sets.Set;
                                     Symbol_Key : in Symbol_Keys;
                                     Number_Map : in Number_Maps.Map) is

      begin -- Conditional_Include
         if Contains (Number_Map, Symbol_Key) then
            Include (Number_Set, Number_Map (Symbol_Key));
         end if; -- Contains (Number_Map, Symbol_Key)
      end Conditional_Include;

      Number_Map : Number_Maps.Map;
      Number_Set : Number_Sets.Set;
      Sum : Natural := 0;

   begin -- Gear_Ratios
      Build (Number_Store, Number_Map);
      for S in Iterate (Symbol_Store) loop
         if Element (S) = '*' then
            Clear (Number_Set);
            Conditional_Include (Number_Set, (Key (S).X - 1, Key (S).Y - 1),
                                 Number_Map);
            Conditional_Include (Number_Set, (Key (S).X - 1, Key (S).Y),
                                 Number_Map);
            Conditional_Include (Number_Set, (Key (S).X - 1, Key (S).Y + 1),
                                 Number_Map);
            Conditional_Include (Number_Set, (Key (S).X, Key (S).Y - 1),
                                 Number_Map);
            Conditional_Include (Number_Set, (Key (S).X, Key (S).Y + 1),
                                 Number_Map);
            Conditional_Include (Number_Set, (Key (S).X + 1, Key (S).Y - 1),
                                 Number_Map);
            Conditional_Include (Number_Set, (Key (S).X + 1, Key (S).Y),
                                 Number_Map);
            Conditional_Include (Number_Set, (Key (S).X + 1, Key (S).Y + 1),
                                 Number_Map);
            if Length (Number_Set) = 2 then
               Sum := Sum + Number_Store (First_Element (Number_Set)).Value *
                 Number_Store (Last_Element (Number_Set)).Value;
            end if; -- Length (Number_Set) = 2
         end if; -- Element (S) = '*'
      end loop; -- S in Iterate (Symbol_Store)
      return Sum;
   end Gear_Ratios;

   Number_Store : Number_Stores.Vector;
   Symbol_Store : Symbol_Stores.Map;
   Sum : Natural;

begin -- December_03
   Read_input (Number_Store, Symbol_Store);
   Sum := 0;
   for N in Iterate (Number_Store) loop
      if Is_Part_Number (Element (N), Symbol_Store) then
         Sum := Sum + Element (N).Value;
      end if; -- Is_Part_Number (Element (N), Symbol_Store)
   end loop; -- N in Iterate (Number_Store)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" & Gear_Ratios (Number_Store, Symbol_Store)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_03;
