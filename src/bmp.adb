with Ada.Streams.Stream_IO;
use Ada.Streams.Stream_IO;

package body Bmp is

   type BMP_Header_Type is record
      Signature    : Unsigned_16;
      File_Size    : Unsigned_32;
      Offset_Pixels : Unsigned_32;
   end record;

   type DIB_Header_Type is record
      Header_Size : Unsigned_32;
      Width       : Unsigned_32;
      Height      : Unsigned_32;
      Plane_Count : Unsigned_16;
      BPP         : Unsigned_16;
      Compression : Unsigned_32;
      Image_Size  : Unsigned_32;
   end record;

   function Read_BMP_Header
      (S : Ada.Streams.Stream_IO.Stream_Access)
       return BMP_Header_Type
   is
      BMP_Header : BMP_Header_Type;
   begin
      Unsigned_16'Read (S, BMP_Header.Signature);
      Unsigned_32'Read (S, BMP_Header.File_Size);
      Unsigned_32'Read (S, BMP_Header.Offset_Pixels);
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
   begin
      if Signature = 16#4D42# then
         Valid := True;
      end if;
      return Valid;
   end Check_Signature;

   function Load_Image (Filename : String)
      return BMP_Type
   is
      BMP           : BMP_Type;
      File          : Ada.Streams.Stream_IO.File_Type;
      BMP_Header    : BMP_Header_Type;
      DIB           : DIB_Header_Type;
      C             : Unsigned_8;
      S             : Ada.Streams.Stream_IO.Stream_Access;
   begin
      Ada.Streams.Stream_IO.Open
         (File, Ada.Streams.Stream_IO.In_File, Filename);
      S := Ada.Streams.Stream_IO.Stream (File);
      BMP_Header := Read_BMP_Header (S);
      if Check_Signature (BMP_Header) then
         raise Program_Error with "Invalid bimap signature.";
      end if;
      DIB := Read_DIB_Header (S);
      BMP.Data.Reserve_Capacity
         (Ada.Containers.Count_Type (DIB.Image_Size));
      Ada.Streams.Stream_IO.Set_Index
         (File,
          Ada.Streams.Stream_IO.Count (BMP_Header.Offset_Pixels));
      for I in 0 .. DIB.Image_Size loop
         Unsigned_8'Read (S, C);
         BMP.Data.Append (C);
      end loop;
      Ada.Streams.Stream_IO.Close (File);
      return BMP;
   end Load_Image;

   function Get_Data (BMP : in out BMP_Type)
      return BMP_Data
   is
   begin
      return BMP.Data;
   end Get_Data;

end Bmp;
