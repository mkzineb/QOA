with Qoaconv;     use Qoaconv;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
procedure Qoaproject is
   Qoa_D            : Qoa_Description;
   Sample_data_Read : Audio_Buffer_Access;
   procedure Free is new Ada.Unchecked_Deallocation
     (Audio_Buffer, Audio_Buffer_Access);
   Bytes_Written : Integer;
begin

   Qoaconv_Wav_Read
     ("/home/moubarik/Desktop/QOA/test_samples/bandcamp/darkside_narrow_road.wav",
      Qoa_D, Sample_data_Read);
   Qoa_Write ("toto.qoa", Sample_data_Read, Qoa_D, Bytes_Written);
   Free (Sample_data_Read);
   Put_Line
     ("size :" & Integer'Image (Bytes_Written / 1_024) & " kb (" &
      Bytes_Written'Img & " bytes)");
end Qoaproject;
