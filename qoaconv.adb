with Ada.Text_IO; use Ada.Text_IO;
package body Qoaconv is
   function Shift_Left (Value : Integer; Amount : Natural) return Integer with
     Import, Convention => Intrinsic;

   function Shift_Right (Value : Integer; Amount : Natural) return Integer with
     Import, Convention => Intrinsic;

   --  TODO : A VOIR SHORT INTEGER FOR V
   --  UnFinished boucle for
   procedure Qoa_Write_U64
     (V : qoa_uint64_t; Bytes : in out Audio_Buffer_Access; P : in out Integer)
   is

   begin
      Bytes (Bytes'First) := Bytes (P);
      null;
   end Qoa_Write_U64;
   --  Finished
   function Qoa_Clamp_s16 (V : Integer) return Integer is
   begin
      if V + 32_768 > 65_535 then
         if V < -32_768 then
            return -32_768;
         end if;
         if V > 32_767 then
            return 32_767;
         end if;
      end if;
      return V;
   end Qoa_Clamp_s16;
   --  Finished
   function Qoa_Lms_Predict (lms : Qoa_Lms_T) return Integer is
      Prediction : Integer := 0;
   begin
      for i in 1 .. Qoa_LMS_Len loop
         Prediction := Prediction + lms.Weight (i) * lms.History (i);
      end loop;
      return Integer (Shift_Right_Arithmetic (Unsigned_64 (Prediction), 13));
   end Qoa_Lms_Predict;
   --  Finished
   procedure Qoa_Lms_Update
     (lms : out Qoa_Lms_T; Sample : Integer; Residual : Integer)
   is
      Delta_q : constant Integer := Shift_Right (Residual, 4);
   begin
      for i in 1 .. Qoa_LMS_Len loop
         lms.Weight (i) :=
           (if lms.History (i) < 0 then lms.Weight (i) - Delta_q
            else lms.Weight (i) + Delta_q);
      end loop;

      for i in 1 .. Qoa_LMS_Len - 3 loop
         lms.History (i) := lms.History (i + 1);
      end loop;
      lms.History (Qoa_LMS_Len - 1) := Sample;
   end Qoa_Lms_Update;
   --  Finished
   function Qoa_Div (V : Integer; ScaleFactor : Integer) return Integer is
      Reciprocal : constant Integer := Qoa_Reciprocal_Tab (ScaleFactor);
      N          : Integer;
      Tmp        : Integer;
   begin
      Tmp := V * Reciprocal + Shift_Left (1, 15);
      N   := Shift_Right (V * Reciprocal + Shift_Left (1, 15), 16);
      N   :=
        N + (Shift_Right (V, 0) - Shift_Left (V, 0)) -
        (Shift_Right (N, 0) - Shift_Left (N, 0));
      return N;
   end Qoa_Div;
   --  Finished
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
      --  Encoded :=  Qoa_Encode (Sample_Data, Qoa_Desc, Unsigned_32(Size));
      if Encoded /= 0 then
         Close (FD);
         return 0;
      end if;
      Result := Write (FD, Encoded'Address, Size);
      Close (FD);
      return Integer (Size);
   end Qoa_Write;
   --  Finished
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
         Qoaconv_Fwrite_U32_Le (Unsigned_32 (Data_Size + 44 - 8), Fd);
      end if;
      return 44 - 8;
   end Qoaconv_Wav_Write;
   --  Finished
   procedure Qoaconv_Fwrite_U32_Le (v : Unsigned_32; Fd : File_Descriptor) is
      type Buf_Type is array (Unsigned_32) of Unsigned_8;
      Buffer : Buf_Type;
      Value  : Integer;
   begin
      Buffer (1) := Unsigned_8 (v and 16#FF#);
      Buffer (2) := Unsigned_8 ((Shift_Right_Arithmetic (v, 8)) and 16#FF#);
      Buffer (3) := Unsigned_8 ((Shift_Right_Arithmetic (v, 16)) and 16#FF#);
      Buffer (4) := Unsigned_8 ((Shift_Right_Arithmetic (v, 24)) and 16#FF#);
      Value      := Write (Fd, Buffer'Address, 4);
      null;
   end Qoaconv_Fwrite_U32_Le;
   --  Finished
   procedure Qoaconv_Fwrite_U16_Le (v : Unsigned_16; Fd : File_Descriptor) is
      type Buf_Type is array (Unsigned_32) of Unsigned_8;
      Buffer : Buf_Type;
      Value  : Integer;
   begin
      Buffer (1) := Unsigned_8 (v and 16#FF#);
      Buffer (2) := Unsigned_8 ((Shift_Right_Arithmetic (v, 8)) and 16#FF#);
      Value      := Write (Fd, Buffer'Address, 2);
   end Qoaconv_Fwrite_U16_Le;
   --  Finished
   function Qoa_Clamp
     (v : Integer; min : Integer; max : Integer) return Integer
   is
   begin
      if v < min then
         return min;
      end if;
      if v > max then
         return max;
      end if;
      return v;
   end Qoa_Clamp;
   --  Finished
   function Qoa_Frame_Size
     (Channels : Unsigned_32; Slices : Unsigned_32) return Unsigned_32
   is
   begin
      return
        8 + Unsigned_32 (Qoa_LMS_Len) * 4 * Channels + 8 * Slices * Channels;
   end Qoa_Frame_Size;
   --  UnFinished
   function Qoa_Encode_Frame
     (Sample_Data : Sample_Buffer; Qoa_Desc : out Qoa_Description;
      Frame_Len   : Unsigned_32; Bytes : out Audio_Buffer_Access)
      return Unsigned_32
   is
      Channels   : Unsigned_32 := Qoa_Desc.Channels;
      P          : Integer;
      Slices     : Unsigned_32 :=
        (Frame_Len + Unsigned_32 (Qoa_Slice_Len) - 1) /
        Unsigned_32 (Qoa_Slice_Len);
      Frame_Size : Unsigned_32 := Qoa_Frame_Size (Channels, Slices);

      type Ch_Buffer is array (1 .. Qoa_Max_Channels) of Integer;
      Prev_ScaleFactor : Ch_Buffer    := (others => 0);
      Weights          : qoa_uint64_t := 0;
      History          : qoa_uint64_t := 0;
      Sample_Index     : Integer      := 0;
      Slice_Len        : Integer      := 0;
      Slice_Start      : Integer      := 0;
      Slice_End        : Integer      := 0;
      Best_Rank        : qoa_uint64_t := -1;
      Best_Error       : qoa_uint64_t := -1;
      Best_Slice       : qoa_uint64_t;
      Best_Lms         : Qoa_Lms_T;
      Best_ScaleFactor : Integer;
      ScaleFactor      : Integer;
      Index            : Natural;
      lms              : Qoa_Lms_T;
      Slice            : qoa_uint64_t;
      Current_Rank     : qoa_uint64_t := 0;
      Current_Error    : qoa_uint64_t := 0;
      Index_Slice      : Natural;
      Si               : Natural;

      Sample        : Integer;
      Predicted     : Integer;
      Residual      : Integer;
      Scaled        : Integer;
      Clamped       : Integer;
      Quantized     : Integer;
      Dequantized   : Integer;
      Reconstructed : Integer;

      Weights_Penalty : Integer;
      Error           : Long_Long_Integer;
      Error_Sq        : qoa_uint64_t;
   begin
      Index       := Natural (Channels);
      Index_Slice := Natural (Slice_End);

      Qoa_Write_U64
        (qoa_uint64_t (Shift_Left (Qoa_Desc.Channels, 56)) or
         qoa_uint64_t (Shift_Left (Qoa_Desc.Samplerate, 32)) or
         qoa_uint64_t (Shift_Left (Frame_Len, 16)) or
         qoa_uint64_t (Shift_Left (Frame_Size, 32)),
         Bytes, P);

      --  write the current lms state
      for i in 1 .. Index loop
         for j in 1 .. Qoa_LMS_Len loop
            History :=
              qoa_uint64_t (Shift_Left (Unsigned_64 (History), 16)) or
              qoa_uint64_t
                (Qoa_Clamp_s16
                   (Integer (qoa_uint64_t (Qoa_Desc.lms (i).History (j)))));
            Weights :=
              qoa_uint64_t (Shift_Left (Unsigned_64 (Weights), 16)) or
              qoa_uint64_t
                (Qoa_Clamp_s16
                   (Integer (qoa_uint64_t (Qoa_Desc.lms (i).Weight (j)))));
         end loop;
         Qoa_Write_U64 (History, Bytes, P);
         Qoa_Write_U64 (Weights, Bytes, P);
      end loop;

      --  encode all samples
      while Sample_Index < Integer (Frame_Len) loop
         for c in 1 .. Index loop
            Slice_Len   :=
              Qoa_Clamp (Qoa_Slice_Len, 0, Integer (Frame_Len) - Sample_Index);
            Slice_Start := Sample_Index * Integer (Channels) + c;
            Slice_End   := (Sample_Index + Slice_Len) * Integer (Channels) + c;

            for sfi in 1 .. 16 loop
               ScaleFactor := (sfi + Prev_ScaleFactor (c)) mod 16;
               lms         := Qoa_Desc.lms (c);
               Slice       := qoa_uint64_t (ScaleFactor);
               Si          := Slice_Start;
               while Si < Slice_End loop
                  Sample    := Integer (Sample_Data (Si));
                  Predicted := Qoa_Lms_Predict (lms);

                  Residual      := Sample - Predicted;
                  Scaled        := Qoa_Div (Residual, ScaleFactor);
                  Clamped       := Qoa_Clamp (Scaled, -8, 8);
                  Quantized     := Qoa_Quant_Tab (Clamped + 8);
                  Dequantized   := Qoa_Dequant_Tab (ScaleFactor * Quantized);
                  Reconstructed := Qoa_Clamp_s16 (Predicted + Dequantized);

                  Weights_Penalty :=
                    Shift_Right
                      (lms.Weight (0) * lms.Weight (0) +
                       lms.Weight (1) * lms.Weight (1) +
                       lms.Weight (2) * lms.Weight (2) +
                       lms.Weight (3) * lms.Weight (3),
                       18) -
                    16#8ff#;

                  if Weights_Penalty < 0 then
                     Weights_Penalty := 0;
                  end if;
                  Error         := Long_Long_Integer (Sample - Reconstructed);
                  Error_Sq      := qoa_uint64_t (Error * Error);
                  Current_Rank  :=
                    Current_Rank + Error_Sq +
                    qoa_uint64_t (Weights_Penalty * Weights_Penalty);
                  Current_Error := Current_Error + Error_Sq;
                  if Current_Rank > Best_Rank then
                     exit;
                  end if;
                  Qoa_Lms_Update (lms, Reconstructed, Dequantized);
                  Slice :=
                    qoa_uint64_t (Shift_Left (Integer (Slice), 3)) or
                    qoa_uint64_t (Quantized);

                  Si := Si + Natural (Channels);
               end loop;
               if Current_Rank < Best_Rank then
                  Best_Rank        := Current_Rank;
                  Best_Error       := Current_Error;
                  Best_Slice       := Slice;
                  Best_Lms         := lms;
                  Best_ScaleFactor := ScaleFactor;
               end if;
            end loop;
            Prev_ScaleFactor (c) := Best_ScaleFactor;
            Qoa_Desc.lms (c)     := Best_Lms;

            Best_Slice :=
              Best_Slice * qoa_uint64_t ((2**(Qoa_Slice_Len - Slice_Len) * 3));
            Qoa_Write_U64 (Best_Slice, Bytes, P);
         end loop;

         Sample_Index := Sample_Index + Qoa_Slice_Len;
      end loop;
      return Unsigned_32 (P);
   end Qoa_Encode_Frame;
   --  Todo : P size
   --  UnFinished
   function Qoa_Encode_Header
     (Qoa_Desc : Qoa_Description; Bytes : out Audio_Buffer_Access)
      return Integer
   is
      P : Integer := 0;
   begin
      Qoa_Write_U64
        (qoa_uint64_t (Shift_Left (Unsigned_64 (Qoa_Magic), 32)) or
         qoa_uint64_t (Qoa_Desc.Samples),
         Bytes, P);
      return P;
   end Qoa_Encode_Header;

   function Qoa_Encode
     (Sample_Data :     Short_Integer; Qoa_Desc : out Qoa_Description;
      Out_Len     : out Unsigned_32) return Natural
   is
      Num_Frames    : Unsigned_32;
      Num_Slices    : Unsigned_32;
      Encoded_Size  : Unsigned_32;
      Bytes         : Integer;
      Header        : Unsigned_32;
      Frame_Len     : Integer;
      Frame_Samples : Unsigned_16;
      Frame_Size    : Unsigned_32;

   begin
      if Qoa_Desc.Channels = 0 or
        Qoa_Desc.Channels > Unsigned_32 (Qoa_Max_Channels) or
        Qoa_Desc.Samples = 0 or Qoa_Desc.Samplerate = 0 or
        Qoa_Desc.Samplerate > 16#FF_FFFF#
      then
         return 0;
      else --  a v jeudi
         null;
      end if;
      Num_Frames   :=
        (Qoa_Desc.Samples + Unsigned_32 (Qoa_Frame_Len - 1)) /
        Unsigned_32 (Qoa_Frame_Len);
      Num_Slices   :=
        (Qoa_Desc.Samples + Unsigned_32 (Qoa_Slice_Len - 1)) /
        Unsigned_32 (Qoa_Slice_Len);
      Encoded_Size :=
        8 + Num_Frames * 8 +
        Num_Frames * Unsigned_32 (Qoa_LMS_Len) * 4 * Qoa_Desc.Channels +
        Num_Slices * 8 * Qoa_Desc.Channels;
      --  for i in 0 .. Integer(Qoa_Desc.Channels) loop
      --     Qoa_Desc.lms(i).Weight(1) := 0;
      --     Qoa_Desc.lms(i).Weight(2) := 0;
      --     Qoa_Desc.lms(i).Weight(3) := - (Shift_Left(1,13));
      --     Qoa_Desc.lms(i).Weight(4) := Shift_Left(1, 14);
      --     for j in 1 .. Qoa_LMS_Len loop
      --        Qoa_Desc.lms(j).History(j) := 0;
      --     end loop;
      --  end loop;

      Frame_Len := Qoa_Frame_Len;
      for Sample_Index in 1 .. Qoa_Desc.Samples loop  --  when Frame_Len loop
         --  Frame_Len     := Qoa_Clamp (Qoa_Frame_Len, 0,
         --  Qoa_Desc.Samples - Integer(Sample_Index));
         --  Frame_Samples := Sample_Data + Sample_Index * Qoa_Desc.Channels;
         --  Frame_Size    := Qoa_Encode_Frame(Frame_Samples, Qoa_Desc,
         --  Frame_Len, Bytes, Header);
         Header := Header + Frame_Size;
      end loop;
      Out_Len := Header;
      return Bytes;
   end Qoa_Encode;
   --  functions to read wav file
   function Qoaconv_Fread_u16_le (Fd : File_Descriptor) return Unsigned_16 is
      Count : Integer;
      type Unsigned_Char is mod 256;
      type buff is array (1 .. 2) of Unsigned_Char;
      Buffer : buff;
   begin
      Count := Read (Fd, Buffer'Address, 2);
      pragma Assert (Count = 2, "16bits not read entirely");
      return
        Shift_Left (Unsigned_16 (Buffer (2)), 8) or Unsigned_16 (Buffer (1));
   end Qoaconv_Fread_u16_le;
   --  Finished
   function Qoaconv_Fread_u32_le (Fd : File_Descriptor) return Unsigned_32 is
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
        Shift_Left (Unsigned_32 (Buffer (2)), 8) or Unsigned_32 (Buffer (1));
   end Qoaconv_Fread_u32_le;
   --  Finished
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
   --  Finished
   function Qoaconv_Wav_Read
     (File_Path : String; Qoa_Desc : out Qoa_Description)
      return Audio_Buffer_Access
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

      RIFF_ID : constant Unsigned_32 := Get_Chunk_Id ("RIFF");
      WAVE_ID : constant Unsigned_32 := Get_Chunk_Id ("WAVE");
      FMT_ID  : constant Unsigned_32 := Get_Chunk_Id ("fmt ");
      DATA_ID : constant Unsigned_32 := Get_Chunk_Id ("data");

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
            else
               null;
            end if;

         elsif Chunk_Type = DATA_ID then
            Data_Size := Chunk_Size;
            exit;
         else
            Lseek (Fd, Long_Integer (Chunk_Size), Seek_Cur);
         end if;
      end loop;
      Qoa_Desc.Channels   := Channels;
      Qoa_Desc.Samplerate := Samplerate;
      Qoa_Desc.Samples    :=
        Data_Size / (Channels * (Unsigned_32 (Bits_Per_Sample) / 8));
      Put_Line ("channels     :" & Unsigned_32'Image (Qoa_Desc.Channels));
      Put_Line ("samplerate   :" & Unsigned_32'Image (Qoa_Desc.Samplerate));
      Put_Line ("samples      :" & Unsigned_32'Image (Qoa_Desc.Samples));
      Put_Line
        ("duration     :" &
         Unsigned_32'Image (Qoa_Desc.Samples / Qoa_Desc.Samplerate));
      declare
         Sample_Count : constant Natural := Natural (Data_Size) / 2;
         Wav_Bytes    : constant Audio_Buffer_Access :=
           new Audio_Buffer (0 .. Sample_Count - 1);
      begin
         Wav_Bytes.all := (others => 0);
         Count := Read (Fd, Wav_Bytes.all'Address, Integer (Data_Size));
         Close (Fd);
         return Wav_Bytes;
      end;
   end Qoaconv_Wav_Read;

end Qoaconv;
