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
   procedure Free is new Ada.Unchecked_Deallocation
     (Audio_Buffer, Audio_Buffer_Access);
   Bytes_Written : Integer;
begin
   Bytes_Written := Qoa_Write ("test.qoa", Sample_data, Qoa_D);
   Free (Sample_data);
end Qoaproject;
