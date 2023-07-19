--  Copyright (C) 2023 Mateus de Lima Oliveira

with Ada.Containers.Vectors;
with Interfaces;
use Interfaces;

package Bmp is

   type BMP_Type is tagged private;

   package BMP_Data_Vectors is new Ada.Containers.Vectors
      (Index_Type => Natural,
       Element_Type => Unsigned_32);

   subtype BMP_Data is BMP_Data_Vectors.Vector;

   function Load_Image (Filename : String)
      return BMP_Type;

   function Get_Data (BMP : in out BMP_Type) return BMP_Data;

   function Get_Width (BMP : in out BMP_Type) return Natural;

   function Get_Height (BMP : in out BMP_Type) return Natural;

   function Get_Plane_Count (BMP : in out BMP_Type) return Natural;

   function Get_Depth (BMP : in out BMP_Type) return Natural;

private

   type BMP_Type is tagged record
      Data          : BMP_Data;
      Width, Height : Natural;
      Plane_Count   : Natural;
      Depth         : Natural;
   end record;

end Bmp;
