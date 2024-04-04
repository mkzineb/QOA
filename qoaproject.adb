with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;  use Interfaces;
with Qoaconv;     use Qoaconv;

procedure Qoaproject is
   Qoa_D : Qoa_Description;

   --  function Qoa_Encode
   --    (Sample_Data : Short_Integer; Qoa_Desc : out Qoa_Description;
   --     Out_Len     : out Unsigned_32) return Natural
   --  is
   --  Num_Frames     : Unsigned_32;
   --  Num_Slices     : Unsigned_32;
   --  Encoded_Size   : Unsigned_32;
   --  Bytes          : Integer;
   --  Header         : Unsigned_32;
   --  Frame_Len      : Unsigned_32;
   --  Frame_Samples  : Unsigned_16;
   --  Frame_Size     : Unsigned_32;
  
   --  begin
   --     if Qoa_Desc.Channels = 0 or Qoa_Desc.Channels > Unsigned_32(Qoa_Max_Channels) 
   --        or Qoa_Desc.Samples = 0 
   --        or Qoa_Desc.Samplerate = 0 or Qoa_Desc.Samplerate > 16#FFFFFF# then
   --        return 0;
   --     end if;
   --     Num_Frames     := (Qoa_Desc.Samples + Unsigned_32(Qoa_Frame_Len - 1)) / Unsigned_32(Qoa_Frame_Len);
   --     Num_Slices     := (Qoa_Desc.Samples + Unsigned_32(Qoa_Slice_Len - 1)) / Unsigned_32(Qoa_Slice_Len);
   --     Encoded_Size   := 8 + Num_Frames * 8 + Num_Frames * Unsigned_32(Qoa_LMS_Len) * 4 * Qoa_Desc.Channels + Num_Slices * 8 * Qoa_Desc.Channels;
   --     Bytes 
      

   --     for i in 0 .. Integer(Qoa_Desc.Channels) loop
   --        Qoa_Desc.lms(i).Weight(1) := 0;
   --        Qoa_Desc.lms(i).Weight(2) := 0;
   --        Qoa_Desc.lms(i).Weight(3) := - (Shift_Left(1,13));
   --        Qoa_Desc.lms(i).Weight(4) := Shift_Left(1, 14);
   --        for j in 0 .. Qoa_LMS_Len loop
   --           Qoa_Desc.lms(j).History(j) := 0;
   --        end loop;
   --     end loop;

   --     Frame_Len := Qoa_Frame_Len;
   --     for Sample_Index in 0 .. Qoa_Desc.Samples loop --when Frame_Len loop
   --        Frame_Len     := Qoa_Clamp (Qoa_Frame_Len, 0, Qoa_Desc.Samples - Sample_Index);
   --        Frame_Samples := Sample_Data + Sample_Index * Qoa_Desc.Channels;
   --        Frame_Size    := Qoa_Encode_Frame(Frame_Samples, Qoa_Desc, Frame_Len, Bytes, Header);
   --        Header        := Header + Frame_Size;
   --     end loop;
   --     Out_Len := Header;
   --     return Bytes;
   --  end Qoa_Encode;

   --  A VOIR SHORT INTEGER FOR V
   type qoa_uint64_t is mod 2**64;
   type Unsigned_Char is mod 256;
   type Buffer is array (Natural) of Unsigned_Char;
   V : Integer := 255526;
   buff : Buffer;

   procedure Qoa_Write_U64 (V : Qoa_Uint64_t; P : out Natural ; Bytes : out Buffer) is
      begin
         --  Bytes := Bytes + P;
         P := P + 8;
         Bytes(0) := Unsigned_Char(Shift_Right_Arithmetic(Unsigned_64(v), 56) and 16#FF#);
         Bytes(1) := Unsigned_Char(Shift_Right_Arithmetic(Unsigned_64(v), 48) and 16#FF#);
         Bytes(2) := Unsigned_Char(Shift_Right_Arithmetic(Unsigned_64(v), 40) and 16#FF#);
         Bytes(3) := Unsigned_Char(Shift_Right_Arithmetic(Unsigned_64(v), 32) and 16#FF#);
         Bytes(4) := Unsigned_Char(Shift_Right_Arithmetic(Unsigned_64(v), 24) and 16#FF#);
         Bytes(5) := Unsigned_Char(Shift_Right_Arithmetic(Unsigned_64(v), 16) and 16#FF#);
         Bytes(6) := Unsigned_Char(Shift_Right_Arithmetic(Unsigned_64(v),  8) and 16#FF#);
         Bytes(8) := Unsigned_Char(Shift_Right_Arithmetic(Unsigned_64(v),  0) and 16#FF#);

   end Qoa_Write_U64;

   function Qoa_Clamp_s16 (V : Integer) return Integer is
   begin 
      if V + 32768 > 65535 then
         if V < -32768 then return -32768; end if;
         if V > 32767  then return 32767; end if;
      else 
         return 0;
      end if;
   end Qoa_Clamp_s16;

begin
   
   Put_Line
     ("results <^>  :" &
      Unsigned_16'Image
        (Qoaconv_Wav_Read
           ("/home/moubarik/Desktop/master_qoa/darkside.wav", Qoa_D)));
end Qoaproject;
