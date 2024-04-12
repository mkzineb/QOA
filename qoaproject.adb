with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;  use Interfaces;
with Qoaconv;     use Qoaconv;
with Ada.Unchecked_Deallocation;
procedure Qoaproject is
   Qoa_D       : Qoa_Description;
   Sample_data : Audio_Buffer_Access :=
     Qoaconv_Wav_Read
       ("/home/moubarik/Desktop/test_samples/bandcamp/darkside_narrow_road.wav",
        Qoa_D);
   function Shift_Left (Value : Integer; Amount : Natural) return Integer with
     Import, Convention => Intrinsic;
   function Shift_Right (Value : Integer; Amount : Natural) return Integer with
     Import, Convention => Intrinsic;

   procedure Free is new Ada.Unchecked_Deallocation
     (Audio_Buffer, Audio_Buffer_Access);
   Bytes_Written : Integer;

   Bytes : Bytes_Char_Acc;

   Value         : Unsigned_64 := Unsigned_64 (Qoa_Magic);
   V             : Unsigned_64 := Shift_Left (Value, 32);
   P             : Unsigned_32 := 0;
   S             : Unsigned_32 := 0;
   Num_Frames    : Unsigned_32;
   Num_Slices    : Unsigned_32;
   Result_header : Unsigned_32;
begin
   Num_Frames :=
     (Qoa_D.Samples + Unsigned_32 (Qoa_Frame_Len - 1)) /
     Unsigned_32 (Qoa_Frame_Len);
   Num_Slices :=
     (Qoa_D.Samples + Unsigned_32 (Qoa_Slice_Len - 1)) /
     Unsigned_32 (Qoa_Slice_Len);
   S          :=
     8 + Num_Frames * 8 +
     Num_Frames * Unsigned_32 (Qoa_LMS_Len) * 4 * Qoa_D.Channels +
     Num_Slices * 8 * Qoa_D.Channels;
   Bytes      := new Bytes_Char (0 .. Integer (S));

   Qoa_Write_U64 (V, Bytes, P);
   Result_header := Qoa_Encode_Header (Qoa_D, Bytes);
   Put_Line ("header p =" & Unsigned_32'Image (Result_header));
   Bytes_Written := Qoa_Write ("test.qoa", Sample_data, Qoa_D);
   Free (Sample_data);
end Qoaproject;
