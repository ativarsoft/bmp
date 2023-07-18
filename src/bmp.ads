with Ada.Containers.Vectors;
with Interfaces;
use Interfaces;

package Bmp is

   type BMP_Type is tagged private;

   subtype Byte is Unsigned_8;

   package BMP_Data_Vectors is new Ada.Containers.Vectors
      (Index_Type => Natural,
       Element_Type => Byte);

   subtype BMP_Data is BMP_Data_Vectors.Vector;

   function Load_Image (Filename : String)
      return BMP_Type;

   function Get_Data (BMP : in out BMP_Type) return BMP_Data;

private

   type BMP_Type is tagged record
      Data : BMP_Data;
   end record;

end Bmp;
