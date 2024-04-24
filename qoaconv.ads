with GNAT.OS_Lib; use GNAT.OS_Lib;
with Interfaces;  use Interfaces;
package Qoaconv with
  SPARK_Mode => off
is
   Qoa_Min_FileSize    : constant Integer := 16;
   Qoa_Max_Channels    : constant Integer := 8;
   Qoa_Slice_Len       : constant Integer := 20;
   Qoa_Slice_Per_Frame : constant Integer := 256;
   Qoa_LMS_Len         : constant Integer := 4;
   Qoa_Frame_Len : constant Integer := (Qoa_Slice_Per_Frame * Qoa_Slice_Len);
   Qoa_Magic           : constant Integer := 16#716F_6166#;

   type History_T is array (0 .. Qoa_LMS_Len - 1) of Integer_16;
   type Weights_T is array (0 .. Qoa_LMS_Len - 1) of Integer_16;

   type Channel is mod 256;
   type Rate is mod 21;
   type Sample is mod 16;

   type Qoa_Lms_T is record
      History : History_T := (others => 0);
      Weight  : Weights_T := (others => 0);
   end record;

   type Lms_T is array (0 .. Qoa_Max_Channels - 1) of Qoa_Lms_T;

   type Qoa_Description is record
      Channels   : Unsigned_32;
      Samplerate : Unsigned_32;
      Samples    : Unsigned_32;
      lms        : Lms_T;
   end record;

   type Unsigned_Char is mod 256;
   type Bytes_Char is array (Natural range <>) of Unsigned_Char;
   type Bytes_Char_Acc is not null access all Bytes_Char;

   type My_Int_Array is array (0 .. 16) of Integer;
   Qoa_Quant_Tab : My_Int_Array :=
     (7, 7, 7, 5, 5, 3, 3, 1, 0, 0, 2, 2, 4, 4, 6, 6, 6);

   type My_Int_Array_16 is array (0 .. 15) of Integer;

   Qoa_ScaleFactor_Tab : My_Int_Array_16 :=
     (1, 7, 21, 45, 84, 138, 211, 304, 421, 562, 731, 928, 1_157, 1_419, 1_715,
      2_048);
   Qoa_Reciprocal_Tab  : My_Int_Array_16 :=
     (65_536, 9_363, 3_121, 1_457, 781, 475, 311, 216, 156, 117, 90, 71, 57,
      47, 39, 32);

   type Audio_Buffer is array (Integer range <>) of Integer_16;
   type Audio_Buffer_Access is access all Audio_Buffer;

   type Qoa_Dequant_Tab_Type is array (0 .. 127) of Integer;
   Qoa_Dequant_Tab : Qoa_Dequant_Tab_Type :=
     (1, -1, 3, -3, 5, -5, 7, -7, 5, -5, 18, -18, 32, -32, 49, -49, 16, -16,
      53, -53, 95, -95, 147, -147, 34, -34, 113, -113, 203, -203, 315, -315,
      63, -63, 210, -210, 378, -378, 588, -588, 104, -104, 345, -345, 621,
      -621, 966, -966, 158, -158, 528, -528, 950, -950, 1_477, -1_477, 228,
      -228, 760, -760, 1_368, -1_368, 2_128, -2_128, 316, -316, 1_053, -1_053,
      1_895, -1_895, 2_947, -2_947, 422, -422, 1_405, -1_405, 2_529, -2_529,
      3_934, -3_934, 548, -548, 1_828, -1_828, 3_290, -3_290, 5_117, -5_117,
      696, -696, 2_320, -2_320, 4_176, -4_176, 6_496, -6_496, 868, -868, 2_893,
      -2_893, 5_207, -5_207, 8_099, -8_099, 1_064, -1_064, 3_548, -3_548,
      6_386, -6_386, 9_933, -9_933, 1_286, -1_286, 4_288, -4_288, 7_718,
      -7_718, 12_005, -12_005, 1_536, -1_536, 5_120, -5_120, 9_216, -9_216,
      14_336, -14_336);

   procedure Qoa_Encode_Header
     (Qoa_Desc :        Qoa_Description; Bytes : out Bytes_Char_Acc;
      P        : in out Unsigned_32);

   procedure Qoa_Write_U64
     (V : Unsigned_64; Bytes : in out Bytes_Char_Acc; P : in out Unsigned_32);
   --     with
   --    Pre  =>
   --     Bytes'First <= Bytes'Last and then P >= Unsigned_32 (Bytes'First)
   --     and then Bytes'Last - Bytes'First >= 7
   --     and then P <= Unsigned_32 (Bytes'Last - 8),
   --    Post => P = P'Old + 8;

   function Qoa_Clamp_s16 (V : Integer) return Integer;

   function Qoa_Lms_Predict (lms : Qoa_Lms_T) return Integer;

   procedure Qoa_Lms_Update
     (lms : out Qoa_Lms_T; Sample : Integer; Residual : Integer);

   procedure Qoaconv_Fwrite_U32_Le (v : Unsigned_32; Fd : File_Descriptor);

   procedure Qoaconv_Fwrite_U16_Le (v : Unsigned_16; Fd : File_Descriptor);

   function Qoa_Frame_Size
     (Channels : Unsigned_32; Slices : Unsigned_32) return Unsigned_32;

   function Qoa_Clamp
     (v : Integer; min : Integer; max : Integer) return Integer;

   procedure Qoa_Write
     (File_Path :     String; Sample_Data : Audio_Buffer_Access;
      Qoa_Desc  : out Qoa_Description; Bytes_writte : out Integer);

   function Get_Chunk_Id (S : String) return Unsigned_32;

   --  reading wav file
   function Qoaconv_Fread_u16_le (Fd : File_Descriptor) return Unsigned_16;

   function Qoaconv_Fread_u32_le (Fd : File_Descriptor) return Unsigned_32;

   procedure Qoaconv_Wav_Read
     (File_Path        :     String; Qoa_Desc : out Qoa_Description;
      Sample_data_Read : out Audio_Buffer_Access);

   procedure Qoa_Encode_Frame
     (Sample_Data :        Audio_Buffer_Access; Frame_Samples : Integer;
      Qoa_Desc    : in out Qoa_Description; Frame_Len : Unsigned_32;
      Bytes       :    out Bytes_Char_Acc; P : in out Unsigned_32);

   function Qoa_Div (V : Integer; ScaleFactor : Integer) return Integer;

   procedure Qoa_Encode
     (Sample_Data :     Audio_Buffer_Access; Qoa_Desc : in out Qoa_Description;
      Out_Len     : out Unsigned_32; Encode : out Bytes_Char_Acc);

end Qoaconv;
