with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with GNAT.OS_Lib;
package body Qoaconv with
  SPARK_Mode => off
is
   function u_64 is new Ada.Unchecked_Conversion (Integer_64, Unsigned_64);
   function i_64 is new Ada.Unchecked_Conversion (Unsigned_64, Integer_64);
   --  warning
   procedure Qoa_Write_U64
     (V : Unsigned_64; Bytes : in out Bytes_Char_Acc; P : in out Unsigned_32)
   is
      Index : constant Integer := 8;
   begin
      for i in 0 .. Index - 1 loop
         --pragma Loop_Invariant (P = P'Loop_Entry + Unsigned_32 (i));
         Bytes.all (Integer (P)) :=
           Unsigned_Char ((V / (2**((Index - i - 1) * 8))) mod 256);
         P                       := P + 1;
      end loop;
   end Qoa_Write_U64;

   function Shift_Left (Value : Integer; Amount : Natural) return Integer with
     Import, Convention => Intrinsic;

   function Shift_Right (Value : Integer; Amount : Natural) return Integer with
     Import, Convention => Intrinsic;

   function Qoa_Clamp_s16 (V : Integer) return Integer is
   begin

      return
        Integer'Min
          (Integer'Max (V, Integer (Integer_16'First)),
           Integer (Integer_16'Last));
      --Integer'Min (V, Integer (Integer_16'Last))
      --Integer'Min (Integer'Max (V, Integer (Integer_16'First)), Integer (Integer_16'Last))
   end Qoa_Clamp_s16;

   procedure print_lms (lms : Qoa_Lms_T) is
   begin
      Put_Line ("-- print lms");
      for i in 0 .. Qoa_LMS_Len - 1 loop
         Put_Line
           ("w: " & Integer_16'Image (lms.Weight (i)) & " h: " &
            Integer_16'Image (lms.History (i)));
      end loop;
   end print_lms;

   function Qoa_Lms_Predict (lms : Qoa_Lms_T) return Integer is
      Prediction : Integer_64 := 0;
   begin
      --print_lms (lms);
      for i in 0 .. Qoa_LMS_Len - 1 loop
         Prediction :=
           Prediction +
           Integer_64 (lms.Weight (i)) * Integer_64 (lms.History (i));
      end loop;
      return Integer ((Prediction - (Prediction mod 2**13)) / 2**13);
   end Qoa_Lms_Predict;

   procedure Qoa_Lms_Update
     (lms : out Qoa_Lms_T; Sample : Integer; Residual : Integer)
   is
      Delta_q : Integer_16 :=
        Integer_16 ((Residual - (Residual mod 2**4)) / 2**4);
   begin
      --  Put_Line ("-- update lms ");
      --  print_lms (lms);
      --  Put_Line ("   sample: " & Sample'Img);
      --  Put_Line ("   residual: " & Residual'Img);
      --  Put_Line ("   delta " & Delta_q'Img);
      --Put_Line (Sample'Img);
      for i in 0 .. Qoa_LMS_Len - 1 loop
         if lms.History (i) < 0 then
            lms.Weight (i) := lms.Weight (i) - Delta_q;
         else
            lms.Weight (i) := lms.Weight (i) + Delta_q;
         end if;
      end loop;

      for i in 0 .. Qoa_LMS_Len - 2 loop
         lms.History (i) := lms.History (i + 1);
      end loop;
      lms.History (Qoa_LMS_Len - 1) := Integer_16 (Sample);
      --print_lms (lms);
   end Qoa_Lms_Update;

   function Qoa_Div (V, Scalefactor : Integer) return Integer is
      Reciprocal : constant Integer      := Qoa_Reciprocal_Tab (Scalefactor);
      Tmp        : constant Long_Integer :=
        Long_Integer (V) * Long_Integer (Reciprocal) + (1 * (2**15));
      N : constant Integer      := Integer ((Tmp - (Tmp mod 2**16)) / 2**16);
   begin
      if V > 0 and then N < 0 then
         return N + 2;
      elsif V < 0 and then N > 0 then
         return N - 2;
      elsif V > 0 and then N = 0 then
         return N + 1;
      elsif V < 0 and then N = 0 then
         return N - 1;
      elsif V = 0 and then N > 0 then
         return N - 1;
      elsif V = 0 and then N < 0 then
         return N + 1;
      else
         return N;
      end if;
   end Qoa_Div;

   procedure Qoa_Write
     (File_Path :     String; Sample_Data : Audio_Buffer_Access;
      Qoa_Desc  : out Qoa_Description; Bytes_writte : out Integer) with
     SPARK_Mode => off
   is
      Num_Frames   : Unsigned_32;
      Num_Slices   : Unsigned_32;
      FD           : File_Descriptor;
      Size         : Natural;
      Result       : Integer;
      Encoded_Size : Unsigned_32;
   begin
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
      FD           := Create_File (File_Path, Binary);
      declare
         Encoded : Bytes_Char_Acc :=
           new Bytes_Char'(0 .. Integer (Encoded_Size) - 1 => 0);
      begin
         if FD = Invalid_FD then
            Put_Line (Errno_Message);
         end if;
         Qoa_Encode (Sample_Data, Qoa_Desc, Unsigned_32 (Size), Encoded);
         --  for i in 0 .. 9 loop
         --     Put_Line ("char :" & Character'Val (Encoded (i)));
         --  end loop;

         if Encoded = null then
            Close (FD);
         end if;
         Result := Write (FD, Encoded.all'Address, Size);
         Close (FD);
         Bytes_writte := Size;
      end;
   end Qoa_Write;

   procedure Qoaconv_Fwrite_U32_Le (v : Unsigned_32; Fd : File_Descriptor) with
     SPARK_Mode => off
   is
      type Buf_Type is array (0 .. 3) of Unsigned_8;
      Buffer : Buf_Type;
      Value  : Integer;
   begin
      Buffer (0) := Unsigned_8 (v and 16#FF#);
      Buffer (1) := Unsigned_8 ((Shift_Right_Arithmetic (v, 8)) and 16#FF#);
      Buffer (2) := Unsigned_8 ((Shift_Right_Arithmetic (v, 16)) and 16#FF#);
      Buffer (3) := Unsigned_8 ((Shift_Right_Arithmetic (v, 24)) and 16#FF#);
      Value      := Write (Fd, Buffer'Address, 4);
   end Qoaconv_Fwrite_U32_Le;

   procedure Qoaconv_Fwrite_U16_Le (v : Unsigned_16; Fd : File_Descriptor) with
     SPARK_Mode => off
   is
      type Buf_Type is array (0 .. 1) of Unsigned_8;
      Buffer : Buf_Type;
      Value  : Integer;
   begin
      Buffer (0) := Unsigned_8 (v and 16#FF#);
      Buffer (1) := Unsigned_8 ((Shift_Right_Arithmetic (v, 8)) and 16#FF#);
      Value      := Write (Fd, Buffer'Address, 2);
   end Qoaconv_Fwrite_U16_Le;

   function Qoa_Clamp
     (v : Integer; min : Integer; max : Integer) return Integer
   is
   begin
      if v < min then
         return min;
      end if;
      if v > max then
         return max;
      else
         return v;
      end if;
   end Qoa_Clamp;

   function Qoa_Frame_Size
     (Channels : Unsigned_32; Slices : Unsigned_32) return Unsigned_32
   is
   begin
      return
        8 + Unsigned_32 (Qoa_LMS_Len) * 4 * Channels + 8 * Slices * Channels;
   end Qoa_Frame_Size;

   procedure Qoa_Encode_Frame
     (Sample_Data :        Audio_Buffer_Access; Frame_Samples : Integer;
      Qoa_Desc    : in out Qoa_Description; Frame_Len : Unsigned_32;
      Bytes       :    out Bytes_Char_Acc; P : in out Unsigned_32)
   is
      Channels   : constant Unsigned_32 := Qoa_Desc.Channels;
      Slices     : constant Unsigned_32 :=
        (Frame_Len + Unsigned_32 (Qoa_Slice_Len) - 1) /
        Unsigned_32 (Qoa_Slice_Len);
      Frame_Size : constant Unsigned_32 := Qoa_Frame_Size (Channels, Slices);

      type Ch_Buffer is array (0 .. Qoa_Max_Channels - 1) of Integer;
      Prev_ScaleFactor : Ch_Buffer := (others => 0);

      Sample_Index     : Integer     := 0;
      Slice_Len        : Integer     := 0;
      Slice_Start      : Integer     := 0;
      Slice_End        : Integer     := 0;
      Best_Rank        : Unsigned_64;
      Best_Error       : Unsigned_64;
      Best_Slice       : Unsigned_64;
      Best_Lms         : Qoa_Lms_T;
      Best_ScaleFactor : Integer;
      ScaleFactor      : Integer;
      lms              : Qoa_Lms_T;
      Slice            : Unsigned_64;
      Current_Rank     : Unsigned_64 := 0;
      Current_Error    : Unsigned_64 := 0;
      Si               : Integer;

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
      Error_Sq        : Unsigned_64;
   begin
      Qoa_Write_U64
        ((Shift_Left (Unsigned_64 (Qoa_Desc.Channels), 56)) or
         (Shift_Left (Unsigned_64 (Qoa_Desc.Samplerate), 32)) or
         (Shift_Left (Unsigned_64 (Frame_Len), 16)) or
         ((Unsigned_64 (Frame_Size))),
         Bytes, P);
      --  Put_Line (Character'Image (Character'Val (Bytes (P))));
      --  Put_Line (Character'Image (Character'Val (Bytes (P + 1))));
      --  Put_Line (Character'Image (Character'Val (Bytes (P + 2))));
      --  Put_Line (Character'Image (Character'Val (Bytes (P + 3))));
      --  Put_Line (Character'Image (Character'Val (Bytes (P + 4))));
      --  Put_Line (Character'Image (Character'Val (Bytes (P + 5))));
      --  Put_Line (Character'Image (Character'Val (Bytes (P + 6))));
      --  Put_Line (Character'Image (Character'Val (Bytes (P + 7))));
      --  write the current lms state
      for c in 0 .. Natural (Channels) - 1 loop
         declare
            Weights : Unsigned_64 := 0;
            History : Unsigned_64 := 0;
         begin
            for i in 0 .. Qoa_LMS_Len - 1 loop
               --print_lms (Qoa_Desc.lms (i));
               History :=
                 Shift_Left (History, 16) or
                 (u_64 (Integer_64 (Qoa_Desc.lms (c).History (i))) and
                  16#FFFF#);

               Weights :=
                 Shift_Left (Weights, 16) or
                 (u_64 (Integer_64 (Qoa_Desc.lms (c).Weight (i))) and
                  16#FFFF#);
               --Put_Line ("history " & History'Img);
               --Put_Line ("weights " & Weights'Img);
            end loop;
            Qoa_Write_U64 (History, Bytes, P);
            Qoa_Write_U64 (Weights, Bytes, P);

         end;

      end loop;

      --  encode all samples
      while Sample_Index < Integer (Frame_Len) loop
         for c in 0 .. Natural (Channels) - 1 loop
            Slice_Len   :=
              Qoa_Clamp (Qoa_Slice_Len, 0, Integer (Frame_Len) - Sample_Index);
            Slice_Start := Sample_Index * Integer (Channels) + c;
            Slice_End   := (Sample_Index + Slice_Len) * Integer (Channels) + c;
            Best_Rank   := -1;
            Best_Error  := -1;
            for sfi in 0 .. 15 loop
               ScaleFactor   := (sfi + Prev_ScaleFactor (c)) mod 16;
               lms           := Qoa_Desc.lms (c);
               Slice         := Unsigned_64 (ScaleFactor);
               Current_Rank  := 0;
               Current_Error := 0;

               Si := Slice_Start;
               --Put_Line ("sa number :" & sfi'Img);
               while Si < Slice_End loop
                  --Put_Line ("si" & Si'Img);
                  --  --  TODO
                  Sample    := Integer (Sample_Data (Frame_Samples + Si));
                  --Put_Line ("sample" & Sample'Img);
                  --print_lms (lms);
                  Predicted := Qoa_Lms_Predict (lms);
                  --Put_Line ("predicted " & Predicted'Img);

                  Residual := Sample - Predicted;

                  Scaled := Qoa_Div (Residual, ScaleFactor);
                  --Put_Line ("--------------scaled " & Scaled'Img);

                  Clamped := Qoa_Clamp (Scaled, -8, 8);
                  --Put_Line ("calmped " & Clamped'Img);

                  Quantized   := Qoa_Quant_Tab (Clamped + 8);
                  --Put_Line ("quantized " & Quantized'Img);
                  --Put_Line ("scalefactor " & ScaleFactor'Img);
                  --  if ScaleFactor = 0 then
                  --     Dequantized := Qoa_Dequant_Tab (Quantized);
                  --  else
                  Dequantized := Qoa_Dequant_Tab (ScaleFactor * 8 + Quantized);

                  --  end if;
                  --Put_Line ("dequantized " & Dequantized'Img);

                  Reconstructed := Qoa_Clamp_s16 (Predicted + Dequantized);
                  --Put_Line ("reconstructed " & Reconstructed'Img);

                  Weights_Penalty :=
                    (Shift_Right
                       (Integer (lms.Weight (0))**2 +
                        Integer (lms.Weight (1))**2 +
                        Integer (lms.Weight (2))**2 +
                        Integer (lms.Weight (3))**2,
                        18)) -
                    16#8ff#;

                  if Weights_Penalty < 0 then
                     Weights_Penalty := 0;
                  end if;
                  Error := Long_Long_Integer (Sample - Reconstructed);
                  --Put_Line ("error " & Error'Img);
                  --Put_Line ("sample" & Sample'Img);
                  --Put_Line ("recons" & Reconstructed'Img);

                  Error_Sq := Unsigned_64 (Error * Error);
                  --Put_Line (Error_Sq'Img);
                  --Put_Line (Weights_Penalty'Img);

                  Current_Rank  :=
                    Current_Rank + Error_Sq +
                    Unsigned_64 (Weights_Penalty * Weights_Penalty);
                  Current_Error := Current_Error + Error_Sq;
                  --Put_Line (Current_Rank'Img);
                  --Put_Line (Best_Rank'Img);

                  if Current_Rank > Best_Rank then
                     exit;
                  end if;
                  --print_lms (lms);

                  Qoa_Lms_Update (lms, Reconstructed, Dequantized);
                  Slice := (Shift_Left (Slice, 3)) or Unsigned_64 (Quantized);
                  Si    := Si + Natural (Channels);
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
            --print_lms (Best_Lms);
            Best_Slice           :=
              Shift_Left (Best_Slice, (Qoa_Slice_Len - Slice_Len) * 3);
            Put_Line ("best slice:" & Best_Slice'Img);
            Qoa_Write_U64 (Best_Slice, Bytes, P);

         end loop;
         Sample_Index := Sample_Index + Qoa_Slice_Len;
      end loop;
      -- result
   end Qoa_Encode_Frame;

   procedure Qoa_Encode_Header
     (Qoa_Desc :        Qoa_Description; Bytes : out Bytes_Char_Acc;
      P        : in out Unsigned_32)
   is
   begin
      Qoa_Write_U64
        (Shift_Left (Unsigned_64 (Qoa_Magic), 32) or
         Unsigned_64 (Qoa_Desc.Samples),
         Bytes, P);
      --  Put_Line
      --    ("shift" &
      --     Unsigned_64'Image
      --       (Shift_Left (Unsigned_64 (Qoa_Magic), 32) or
      --        Unsigned_64 (Qoa_Desc.Samples)));
   end Qoa_Encode_Header;

   procedure Qoa_Encode
     (Sample_Data :     Audio_Buffer_Access; Qoa_Desc : in out Qoa_Description;
      Out_Len     : out Unsigned_32; Encode : out Bytes_Char_Acc)
   is
      Num_Frames    : Unsigned_32;
      Num_Slices    : Unsigned_32;
      Encoded_Size  : Unsigned_32;
      Frame_Len     : Integer;
      Frame_Samples : Integer;
      P             : Unsigned_32;
      Sample_Index  : Integer := 0;

      Test_D : Integer_32 := 0;
   begin
      if Qoa_Desc.Samples = 0 or Qoa_Desc.Samplerate = 0 or
        Qoa_Desc.Samplerate > 16#FF_FFFF# or Qoa_Desc.Channels = 0 or
        Qoa_Desc.Channels > Unsigned_32 (Qoa_Max_Channels)

      then
         OS_Exit (1);
         --  TODO
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

      declare
         Bytes : Bytes_Char_Acc :=
           new Bytes_Char'(0 .. Integer (Encoded_Size) - 1 => 0);

      begin
         for c in 0 .. Natural (Qoa_Desc.Channels) - 1 loop
            Qoa_Desc.lms (c).Weight (0) := 0;
            Qoa_Desc.lms (c).Weight (1) := 0;
            Qoa_Desc.lms (c).Weight (2) := -1 * (2**13);
            Qoa_Desc.lms (c).Weight (3) := 2**14;
            for j in 0 .. Qoa_LMS_Len - 1 loop
               Qoa_Desc.lms (c).History (j) := 0;
            end loop;
         end loop;
         Qoa_Encode_Header (Qoa_Desc, Bytes, P);
         Frame_Len := Qoa_Frame_Len;
         while Sample_Index < Integer (Qoa_Desc.Samples) loop
            Frame_Len :=
              Qoa_Clamp
                (Qoa_Frame_Len, 0, Integer (Qoa_Desc.Samples) - Sample_Index);

            Frame_Samples :=
              Integer ((Sample_Index) * Integer (Qoa_Desc.Channels));
            Qoa_Encode_Frame
              (Sample_Data, Frame_Samples, Qoa_Desc, Unsigned_32 (Frame_Len),
               Bytes, P);
            -- Todo :Frame_size a supprimer
            Sample_Index := Sample_Index + Frame_Len;
         end loop;
         Out_Len := P;
         Encode  := Bytes;
      end;
   end Qoa_Encode;

   function Qoaconv_Fread_u16_le (Fd : File_Descriptor) return Unsigned_16 with
     SPARK_Mode => off
   is
      Count : Integer;
      type Unsigned_Char is mod 256;
      type buff is array (0 .. 1) of Unsigned_Char;
      Buffer : buff;
   begin
      Count := Read (Fd, Buffer'Address, 2);
      pragma Assert (Count = 2, "16bits not read entirely");
      return
        Shift_Left (Unsigned_16 (Buffer (1)), 8) or Unsigned_16 (Buffer (0));
   end Qoaconv_Fread_u16_le;

   function Qoaconv_Fread_u32_le (Fd : File_Descriptor) return Unsigned_32 with
     SPARK_Mode => off
   is
      Count : Integer;
      type Unsigned_Char is mod 256;
      type buff is array (0 .. 3) of Unsigned_Char;
      Buffer : buff;
   begin
      Count := Read (Fd, Buffer'Address, 4);
      pragma Assert (Count = 4, "32bits not read entirely");
      return
        Shift_Left (Unsigned_32 (Buffer (3)), 24) or
        Shift_Left (Unsigned_32 (Buffer (2)), 16) or
        Shift_Left (Unsigned_32 (Buffer (1)), 8) or Unsigned_32 (Buffer (0));
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

   procedure Qoaconv_Wav_Read
     (File_Path        :     String; Qoa_Desc : out Qoa_Description;
      Sample_data_Read : out Audio_Buffer_Access) with
     SPARK_Mode => off
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
           new Audio_Buffer'(0 .. Sample_Count - 1 => 0);
      begin
         Count := Read (Fd, Wav_Bytes.all'Address, Integer (Data_Size));
         Close (Fd);
         Sample_data_Read := Wav_Bytes;
      end;
   end Qoaconv_Wav_Read;

end Qoaconv;
