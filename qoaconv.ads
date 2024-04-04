with Interfaces; use Interfaces;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package Qoaconv is

    Qoa_Min_FileSize    : constant Integer := 16;
    Qoa_Max_Channels    : constant Integer := 8;
    Qoa_Slice_Len       : constant Integer := 20;
    Qoa_Slice_Per_Frame : constant Integer := 256;
    Qoa_LMS_Len         : constant Integer := 4;
    Qoa_Frame_Len       : constant Integer := (Qoa_Slice_Per_Frame * Qoa_Slice_Len);
    Qoa_Magic           : constant Integer := 16#716F_6166#;
        
    Qoa_Frame_Size : constant Integer := 0; -- TODO

    type History_T is array (1..Qoa_LMS_Len) of Integer;
    type Weights_T is array (1..Qoa_LMS_Len) of Integer;

    type Channel is mod 256;
    type Rate is mod 21;
    type Sample is mod 16;

    type Qoa_Lms_T is record
        History : History_T;
        Weight  : Weights_T;
    end record;

    type Lms_T is array (1..Qoa_LMS_Len) of Qoa_Lms_T;

    type Qoa_Description is record
        Channels   : Unsigned_32;
        Samplerate : Unsigned_32;
        Samples    : Unsigned_32;
        lms        : Lms_T;
    end record;

    function Qoa_Write
     (File_Path : String; Sample_Data : Short_Integer;
      Qoa_Desc  : Qoa_Description) return Integer;

    procedure Qoa_Max_Frame_Size (Qoa : Qoa_Description);

    procedure Qoaconv_Fwrite_U32_Le
     (v : Unsigned_32; Fd : File_Descriptor);

    procedure Qoaconv_Fwrite_U16_Le
     (v : Unsigned_16; Fd : File_Descriptor);

    function Qoa_Clamp (v : Integer; min : Integer; max : Integer) return Integer;

    function Qoaconv_Wav_Write
     (File_Path : String; sample_data : Short_Integer;
      Qoa_Desc  : Qoa_Description) return Integer;

    -- reading wav file
    function Qoaconv_Fread_u16_le
     (Fd : File_Descriptor) return Unsigned_16;

    function Qoaconv_Fread_u32_le
     (Fd : File_Descriptor) return Unsigned_32;

    function Qoaconv_Wav_Read
     (File_Path : String; Qoa_Desc : out Qoa_Description) return Unsigned_16;

    function Qoa_Encode_Frame(Frame_Samples : Unsigned_16; Qoa_Desc : Qoa_Description; Frame_Len : Unsigned_32; Bytes : Integer; Header : Unsigned_32) return Unsigned_32;

    

end Qoaconv;
