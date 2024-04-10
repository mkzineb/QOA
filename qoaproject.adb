with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;  use Interfaces;
with Qoaconv;     use Qoaconv;
with Ada.Unchecked_Deallocation;
with test;        use test;
procedure Qoaproject is
  Qoa_D : Qoa_Description;
  a     : Audio_Buffer_Access :=
   Qoaconv_Wav_Read ("/home/moubarik/Desktop/master_qoa/darkside.wav", Qoa_D);
  procedure Free is new Ada.Unchecked_Deallocation
   (Audio_Buffer, Audio_Buffer_Access);
  --  b : Integer := Qoa_Div (142, 1);

begin
  Free (a);
end Qoaproject;
