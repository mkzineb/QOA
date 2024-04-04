with Ada.Text_IO; use Ada.Text_IO;
package body Qoaconv is
    --  functions to write qoa file
   function Qoa_Write
     (File_Path : String; Sample_Data : Short_Integer;
      Qoa_Desc  : Qoa_Description) return Integer
   is
      FD      : File_Descriptor;
      Encoded : Natural;
      Size    : Natural;
      Result  : Integer;
   begin
      FD := Create_File (File_Path, Binary);
      if FD = Invalid_FD then
         Put_Line (Errno_Message);
         return 0;
      end if;
      -- Encoded :=  Qoa_Encode (Sample_Data, Qoa_Desc, Unsigned_32(Size));
      if Encoded /= 0 then
         Close(Fd);
         return 0;
      end if;
      Result := Write (Fd, Encoded'Address, Size);
      Close (Fd);
      return Integer (Size);
   end Qoa_Write;

   function Qoaconv_Wav_Write
     (File_Path : String; sample_data : Short_Integer;
      Qoa_Desc  : Qoa_Description) return Integer
   is
      Data_Size       : Unsigned_32;
      Samplerate      : Unsigned_32;
      Bits_Per_Sample : Short_Integer;
      Channels        : Unsigned_32;
      Fd              : File_Descriptor;
      Value           : Integer;
   begin
      Data_Size := Qoa_Desc.Samples * Qoa_Desc.Channels * Short_Integer'Size;
      Samplerate      := Qoa_Desc.Samplerate;
      Bits_Per_Sample := 16;
      Channels        := Qoa_Desc.Channels;

      Fd := Open_Read_Write (File_Path, Binary);
      if Fd = Invalid_FD then
         Put_Line (Errno_Message);
      else
         Qoaconv_Fwrite_U32_Le
           (Unsigned_32 (Data_Size + 44 - 8), Fd);
      end if;
      return 44 - 8;
   end Qoaconv_Wav_Write;

    procedure Qoa_Max_Frame_Size (Qoa : Qoa_Description) is
    begin
        null;
    end Qoa_Max_Frame_Size;

   procedure Qoaconv_Fwrite_U32_Le
     (v : Unsigned_32; Fd : File_Descriptor)
   is
      type Buf_Type is array (Unsigned_32) of Unsigned_8;
      Buffer : Buf_Type;
      Value  : Integer;
   begin
      Buffer (1) := Unsigned_8 (v and 16#FF#);
      Buffer (2) := Unsigned_8 ((Shift_Right_Arithmetic (v, 8)) and 16#FF#);
      Buffer (3) := Unsigned_8 ((Shift_Right_Arithmetic (v, 16)) and 16#FF#);
      Buffer (4) := Unsigned_8 ((Shift_Right_Arithmetic (v, 24)) and 16#FF#);
      Value := Write (Fd, Buffer'Address, 4);
      null;
   end Qoaconv_Fwrite_U32_Le;

   procedure Qoaconv_Fwrite_U16_Le
     (v : Unsigned_16; Fd : File_Descriptor)
   is
      type Buf_Type is array (Unsigned_32) of Unsigned_8;
      Buffer : Buf_Type;
      Value  : Integer;
   begin
      Buffer (1) := Unsigned_8 (v and 16#FF#);
      Buffer (2) := Unsigned_8 ((Shift_Right_Arithmetic (v, 8)) and 16#FF#);
      Value := Write (Fd, Buffer'Address, 2);
   end Qoaconv_Fwrite_U16_Le;

   function Qoa_Clamp (v : Integer; min : Integer; max : Integer) return Integer is
   begin
      if v < min then return min; end if;
      if v > max then return max; end if;
      return v;
   end Qoa_Clamp;

   function Qoa_Encode_Frame(Frame_Samples : Unsigned_16; Qoa_Desc : Qoa_Description; Frame_Len : Unsigned_32; Bytes : Integer; Header : Unsigned_32) return Unsigned_32 is
   begin
   return 0;
   end Qoa_Encode_Frame;
    
    --  functions to read wav file
   function Qoaconv_Fread_u16_le
       (Fd : File_Descriptor) return Unsigned_16
    is
        Count : Integer;
        type Unsigned_Char is mod 256;
        type buff is array (1 .. 2) of Unsigned_Char;
        Buffer : buff;
    begin
        Count := Read (Fd, Buffer'Address, 2);
        pragma Assert (Count = 2, "16bits not read entirely");
        return
           Shift_Left (Unsigned_16 (Buffer (2)), 8) or
           Unsigned_16 (Buffer (1));
    end Qoaconv_Fread_u16_le;

   function Qoaconv_Fread_u32_le
       (Fd : File_Descriptor) return Unsigned_32
    is
        Count : Integer;
        type Unsigned_Char is mod 256;
        type buff is array (1 .. 4) of Unsigned_Char;
        Buffer : buff;
    begin
        Count := Read (Fd, Buffer'Address, 4);
        pragma Assert (Count = 4, "32bits not read entirely");
        return
           Shift_Left (Unsigned_32 (Buffer (4)), 24) or
           Shift_Left (Unsigned_32 (Buffer (3)), 16) or
           Shift_Left (Unsigned_32 (Buffer (2)), 8) or
           Unsigned_32 (Buffer (1));
    end Qoaconv_Fread_u32_le;


   function Get_Chunk_Id (S : String) return Unsigned_32 is
   Chunk_ID : Unsigned_32 := 0;
   begin
      Chunk_ID :=
        (Shift_Left (Character'Pos (S (1)), 0)) or
        (Shift_Left (Character'Pos (S (2)), 8)) or
        (Shift_Left (Character'Pos (S (3)), 16)) or
        (Shift_Left (Character'Pos (S (4)), 24));
      return Chunk_ID;
   end Get_Chunk_Id;

   function Qoaconv_Wav_Read
     (File_Path : String; Qoa_Desc : out Qoa_Description) return Unsigned_16
   is
      Fd              : File_Descriptor := 1;
      Count           : Integer         := 0;
      Container_Type  : Unsigned_32     := 0;
      Wav_Size        : Unsigned_32     := 0;
      Data_Size       : Unsigned_32     := 0;
      Format_Length   : Unsigned_16     := 0;
      Format_Type     : Unsigned_16     := 0;
      Byte_Rate       : Unsigned_32     := 0;
      Block_Align     : Unsigned_16     := 0;
      Bits_Per_Sample : Unsigned_16     := 1;
      Chunk_Type      : Unsigned_32     := 0;
      Chunk_Size      : Unsigned_32     := 0;
      Extra_Params    : Unsigned_16     := 0;
      Result          : Integer         := 0;
      Channels        : Unsigned_32     := 0;
      Samplerate      : Unsigned_32     := 0;
      Wav_Id          : Unsigned_32     := 0;

      type buff is array (Unsigned_16) of Unsigned_16;
      Wav_Bytes : buff;

      RIFF_ID   : constant Unsigned_32 := Get_Chunk_Id ("RIFF");
      WAVE_ID   : constant Unsigned_32 := Get_Chunk_Id ("WAVE");
      FMT_ID    : constant Unsigned_32 := Get_Chunk_Id ("fmt ");
      DATA_ID   : constant Unsigned_32 := Get_Chunk_Id ("data");

   begin
      Fd             := Open_Read (File_Path, Binary);
      Container_Type := Qoaconv_Fread_u32_le (Fd);
      if Container_Type /= RIFF_ID then
         raise Program_Error with "Not a RIFF container";
      end if;
      Wav_Size := Qoaconv_Fread_u32_le (Fd);
      Wav_Id   := Qoaconv_Fread_u32_le (Fd);

      if Wav_Id /= WAVE_ID then
         raise Program_Error with "No WAVE id found";
      end if;

      loop
         Chunk_Type := Qoaconv_Fread_u32_le (Fd);
         Chunk_Size := Qoaconv_Fread_u32_le (Fd);
         if Chunk_Type = FMT_ID then
            pragma Assert
              (Chunk_Size = 16 or Chunk_Size = 18,
               "WAV fmt chunk size mismatch");
            Format_Type     := Qoaconv_Fread_u16_le (Fd);
            Channels        := Unsigned_32 (Qoaconv_Fread_u16_le (Fd));
            Samplerate      := Qoaconv_Fread_u32_le (Fd);
            Byte_Rate       := Qoaconv_Fread_u32_le (Fd);
            Block_Align     := Qoaconv_Fread_u16_le (Fd);
            Bits_Per_Sample := Qoaconv_Fread_u16_le (Fd);

            if Chunk_Size = 18 then
               Extra_Params := Qoaconv_Fread_u16_le (Fd);
               pragma Assert
                 (Extra_Params = 0, "WAV fmt extra params not supported");
            end if;

         elsif Chunk_Type = DATA_ID then
            Data_Size := Chunk_Size;
            exit;
            null;
         else
            Lseek (Fd, Long_Integer (Chunk_Size), Seek_Cur);
         end if;
      end loop;
      Qoa_Desc.Channels   := Channels;
      Qoa_Desc.Samplerate := Samplerate;
      Qoa_Desc.Samples    :=
        25_762_824 / (Channels * (Unsigned_32 (Bits_Per_Sample) / 8));
      Put_Line ("channels     :" & Unsigned_32'Image (Qoa_Desc.Channels));
      Put_Line ("samplerate   :" & Unsigned_32'Image (Qoa_Desc.Samplerate));
      Put_Line ("samples      :" & Unsigned_32'Image (Qoa_Desc.Samples));
      Put_Line
        ("duration     :" &
         Unsigned_32'Image (Qoa_Desc.Samples / Qoa_Desc.Samplerate));

      -- Count := Read (Fd, Wav_Bytes'Address, Integer (Data_Size));
      Close (Fd);
      return 0;
   end Qoaconv_Wav_Read;

   
end Qoaconv;