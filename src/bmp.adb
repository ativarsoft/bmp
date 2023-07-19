--  Copyright (C) 2023 Mateus de Lima Oliveira

with Ada.Streams.Stream_IO;
use Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Exceptions;
use Ada.Exceptions;

package body Bmp is

   Invalid_Compression_Value : exception;

   type BMP_Header_Type is tagged record
      Signature    : Unsigned_16;
      File_Size    : Unsigned_32;
      Offset_Pixels : Unsigned_32;
   end record;

   type DIB_Header_Type is tagged record
      Header_Size : Unsigned_32;
      Width       : Unsigned_32;
      Height      : Unsigned_32;
      Plane_Count : Unsigned_16;
      BPP         : Unsigned_16;
      Compression : Unsigned_32;
      Image_Size  : Unsigned_32;
   end record;

   procedure Print (BMP : in out BMP_Header_Type)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("BMP header:");

      Put ("  ");
      Put ("Signature           : ");
      Put (BMP.Signature'Image);
      Put (" ");
      Put ("(");
      case BMP.Signature is
         when 16#4D42# =>
            Put ("BM");
         when others =>
            Put ("Unknown");
      end case;
      Put (")");
      New_Line;

      Put ("  ");
      Put ("File size           : ");
      Put (BMP.File_Size'Image);
      Put (" ");
      Put ("bytes");
      New_Line;

      Put ("  ");
      Put ("Offset of pixel data: ");
      Put_Line (BMP.Offset_Pixels'Image);
   end Print;

   procedure Print (DIB : in out DIB_Header_Type)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("DIB header:");

      Put ("  ");
      Put ("Header size: ");
      Put (DIB.Header_Size'Image);
      Put (" ");
      Put ("(");
      case DIB.Header_Size is
         when 12 =>
            Put ("BITMAPCOREHEADER OS21XBITMAPHEADER");
         when 64 =>
            Put ("OS22XBITMAPHEADER");
         when 16 =>
            Put ("OS22XBITMAPHEADER");
         when 40 =>
            Put ("BITMAPINFOHEADER");
         when 52 =>
            Put ("BITMAPV2INFOHEADER");
         when 56 =>
            Put ("BITMAPV3INFOHEADER");
         when 108 =>
            Put ("BITMAPV4HEADER");
         when 124 =>
            Put ("BITMAPV5HEADER");
         when others =>
            Put ("Unknown");
      end case;
      Put (")");
      New_Line;

      Put ("  ");
      Put ("Width      : ");
      Put (DIB.Width'Image);
      Put (" ");
      Put ("pixels");
      New_Line;

      Put ("  ");
      Put ("Height     : ");
      Put (DIB.Height'Image);
      Put (" ");
      Put ("pixels");
      New_Line;

      Put ("  ");
      Put ("Plane count: ");
      Put (DIB.Plane_Count'Image);
      Put (" ");
      if DIB.Plane_Count = 1 then
         Put ("(OK)");
      else
         Put ("(INVALID)");
      end if;
      New_Line;

      Put ("  ");
      Put ("BPP        : ");
      Put (DIB.BPP'Image);
      Put (" ");
      Put ("bytes");
      New_Line;

      Put ("  ");
      Put ("Compression: ");
      Put (DIB.Compression'Image);
      Put (" ");
      Put ("(");
      case DIB.Compression is
         when 0 =>
            Put ("BI_RGB");
         when 1 =>
            Put ("BI_RLE8");
         when 2 =>
            Put ("BI_RLE4");
         when 3 =>
            Put ("BI_BITFIELDS");
         when 4 =>
            Put ("BI_JPEG");
         when 5 =>
            Put ("BI_PNG");
         when 6 =>
            Put ("BI_ALPHABITFIELDS");
         when 11 =>
            Put ("BI_CMYK");
         when 12 =>
            Put ("BI_CMYKRLE8");
         when 13 =>
            Put ("BI_CMYKRLE4");
         when others =>
            Put ("Unknown");
      end case;
      Put (")");
      New_Line;

      Put ("  ");
      Put ("Image size : ");
      Put (DIB.Image_Size'Image);
      Put (" ");
      Put ("bytes");
      New_Line;
   end Print;

   function Read_BMP_Header
      (S : Ada.Streams.Stream_IO.Stream_Access)
       return BMP_Header_Type
   is
      BMP_Header : BMP_Header_Type;
      Next_16    : Unsigned_16;
      Next_32    : Unsigned_32;
   begin
      Unsigned_16'Read (S, Next_16);
      BMP_Header.Signature := Next_16;
      Unsigned_32'Read (S, Next_32);
      BMP_Header.File_Size := Next_32;
      Unsigned_16'Read (S, Next_16); --  Ignore
      Unsigned_16'Read (S, Next_16); --  Ignore
      Unsigned_32'Read (S, Next_32);
      BMP_Header.Offset_Pixels := Next_32;
      return BMP_Header;
   end Read_BMP_Header;

   function Read_DIB_Header
      (S : Ada.Streams.Stream_IO.Stream_Access)
       return DIB_Header_Type
   is
      DIB : DIB_Header_Type;
   begin
      Unsigned_32'Read (S, DIB.Header_Size);
      if DIB.Header_Size = 12 then
         --  OS/2 BMP
         Unsigned_32'Read (S, DIB.Width);
         Unsigned_32'Read (S, DIB.Height);
         Unsigned_16'Read (S, DIB.Plane_Count);
         Unsigned_16'Read (S, DIB.BPP);
      else
         --  Windows BMP
         Unsigned_32'Read (S, DIB.Width);
         Unsigned_32'Read (S, DIB.Height);
         Unsigned_16'Read (S, DIB.Plane_Count);
         Unsigned_16'Read (S, DIB.BPP);
         Unsigned_32'Read (S, DIB.Compression);
         Unsigned_32'Read (S, DIB.Image_Size);
      end if;
      return DIB;
   end Read_DIB_Header;

   function Check_Signature
      (BMP_Header : BMP_Header_Type)
       return Boolean
   is
      Signature : constant Unsigned_16 := BMP_Header.Signature;
      Valid : Boolean := False;
      Ignore_Signature : constant Boolean := False;
   begin
      if Signature = 16#4D42# then
         Valid := True;
      else
         if Ignore_Signature = True then
            Valid := True;
         end if;
      end if;
      return Valid;
   end Check_Signature;

   package Color_Vectors is new Ada.Containers.Vectors
      (Natural, Unsigned_32);

   type Color_Table_Type is tagged record
      Data : Color_Vectors.Vector;
   end record;

   function Read_Color_Table
      (BMP : in out BMP_Type;
       S   : Ada.Streams.Stream_IO.Stream_Access)
       return Color_Table_Type
   is
      Color_Table : Color_Table_Type;
      Color : Unsigned_32;
      Length : Natural := 2 ** BMP.Get_Depth;
   begin
      for I in 1 .. Length loop
         Unsigned_32'Read (S, Color);
         Color_Table.Data.Append (Color);
      end loop;
      return Color_Table;
   end Read_Color_Table;

   function Get_Color
      (Color_Table : in out Color_Table_Type;
       Index : Natural)
       return Unsigned_32
   is
   begin
      return Color_Table.Data (Index);
   end Get_Color;

   procedure Read_RGB32_Data
      (BMP        : in out BMP_Type;
       S          : Ada.Streams.Stream_IO.Stream_Access;
       Image_Size : Natural)
   is
      C             : Unsigned_32;
   begin
      for I in 1 .. Image_Size loop
         Unsigned_32'Read (S, C);
         BMP.Data.Append (C);
      end loop;
   end Read_RGB32_Data;

   procedure Read_Uncompressed_Data
      (BMP        : in out BMP_Type;
       S          : Ada.Streams.Stream_IO.Stream_Access;
       Image_Size : Natural)
   is
   begin
      case BMP.Depth is
         when 32 =>
            BMP.Read_RGB32_Data (S, Image_Size);
         when others =>
            raise Program_Error with "Unsupported image depth.";
      end case;
   end Read_Uncompressed_Data;

   procedure Read_RLE8_Data
      (BMP         : in out BMP_Type;
       S           : Ada.Streams.Stream_IO.Stream_Access;
       Image_Size  : Natural;
       Width       : Natural;
       Color_Table : in out Color_Table_Type)
   is
      use Ada.Text_IO;

      Runlength     : Unsigned_8;
      C             : Unsigned_8;
      Color         : Unsigned_32;
      I             : Natural := 0;
      Line_Position : Natural := 0;
      Delta_Compression_Value : Unsigned_16;
      Absolute_Mode_Length    : Unsigned_8;
   begin
      while I < Image_Size loop
         Unsigned_8'Read (S, Runlength);
         if Runlength = 0 then
            Unsigned_8'Read (S, C);
            case C is
               when 0 =>
                  Put_Line ("End of line.");
                  for J in 1 .. Width - Line_Position loop
                     BMP.Data.Append (0);
                  end loop;
               when 1 =>
                  Put_Line ("End of bitmap.");
                  return;
               when 2 =>
                  Put_Line ("Delta");
                  Unsigned_16'Read (S, Delta_Compression_Value);
                  for J in 1 .. Natural (Delta_Compression_Value) loop
                     BMP.Data.Append (0);
                  end loop;
                  --  raise Program_Error with "Delta compression is currently unimplemented.";
               when others =>
                  --  Absolute mode
                  Absolute_Mode_Length := C;
                  for J in 1 .. Absolute_Mode_Length loop
                     Unsigned_8'Read (S, C);
                     Color := Color_Table.Get_Color (Natural (C));
                     BMP.Data.Append (Color);
                  end loop;
                  Line_Position := Line_Position + Natural (Runlength);
                  if Line_Position >= Width then
                     Line_Position := Line_Position - Width;
                  end if;
                  I := I + Natural (Runlength);
            end case;
            else
               for J in 1 .. Runlength loop
                  Unsigned_8'Read (S, C);
                  Color := Color_Table.Get_Color (Natural (C));
                  BMP.Data.Append (Color);
               end loop;
         end if;
      end loop;
   exception
      when E : others =>
         Put_Line (Exception_Message (E));
         Put_Line ("Error reading pixel data at position " & I'Image & ".");
   end Read_RLE8_Data;

   function Load_Image (Filename : String)
      return BMP_Type
   is
      BMP           : BMP_Type;
      File          : Ada.Streams.Stream_IO.File_Type;
      BMP_Header    : BMP_Header_Type;
      DIB           : DIB_Header_Type;
      S             : Ada.Streams.Stream_IO.Stream_Access;
   begin
      Ada.Text_IO.Put ("Loading bitmap: ");
      Ada.Text_IO.Put_Line (Filename);
      Ada.Streams.Stream_IO.Open
         (File, Ada.Streams.Stream_IO.In_File, Filename);
      S := Ada.Streams.Stream_IO.Stream (File);
      BMP_Header := Read_BMP_Header (S);
      BMP_Header.Print;
      if Check_Signature (BMP_Header) = False then
         raise Program_Error with "Invalid bimap signature.";
      end if;
      DIB := Read_DIB_Header (S);
      DIB.Print;
      BMP.Width := Natural (DIB.Width);
      BMP.Height := Natural (DIB.Height);
      BMP.Plane_Count := Natural (DIB.Plane_Count);
      BMP.Depth := Natural (DIB.BPP);
      BMP.Data.Reserve_Capacity
         (Ada.Containers.Count_Type (DIB.Image_Size));
      Ada.Streams.Stream_IO.Set_Index
         (File,
          Ada.Streams.Stream_IO.Count (BMP_Header.Offset_Pixels));
      case DIB.Compression is
         when 0 =>
            BMP.Read_Uncompressed_Data
               (S, Natural (DIB.Image_Size));
         when 1 =>
            declare
               Color_Table : Color_Table_Type;
            begin
               Color_Table := BMP.Read_Color_Table (S);
               BMP.Read_RLE8_Data
                  (S, Natural (DIB.Image_Size), Natural (DIB.Width), Color_Table);
            end;
         when others =>
            raise Invalid_Compression_Value with "Compression value " & DIB.Compression'Image & " is unrecognized.";
      end case;
      Ada.Streams.Stream_IO.Close (File);
      return BMP;
   end Load_Image;

   function Get_Data (BMP : in out BMP_Type)
      return BMP_Data
   is
   begin
      return BMP.Data;
   end Get_Data;

   function Get_Width
      (BMP : in out BMP_Type)
       return Natural
   is
   begin
      return BMP.Width;
   end Get_Width;

   function Get_Height
      (BMP : in out BMP_Type)
       return Natural
   is
   begin
      return BMP.Height;
   end Get_Height;

   function Get_Plane_Count
      (BMP : in out BMP_Type)
       return Natural
   is
   begin
      return BMP.Plane_Count;
   end Get_Plane_Count;

   function Get_Depth
      (BMP : in out BMP_Type)
       return Natural
   is
   begin
      return BMP.Depth;
   end Get_Depth;

end Bmp;
