with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;  use Interfaces;
with Qoaconv;     use Qoaconv;

procedure Qoaproject is
   Qoa_D : Qoa_Description;
begin
   
   Put_Line
     ("results <^>  :" &
      Unsigned_16'Image
        (Qoaconv_Wav_Read
           ("/home/moubarik/Desktop/master_qoa/darkside.wav", Qoa_D)));
end Qoaproject;
