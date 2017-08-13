(in-package :oss)

(include "sys/soundcard.h")

;; Audio Formats
(constant (+afmt-query+ "AFMT_QUERY"))
(constant (+afmt-mu-law+ "AFMT_MU_LAW"))
(constant (+afmt-a-law+ "AFMT_A_LAW"))
(constant (+afmt-ima-adpcm+ "AFMT_IMA_ADPCM"))
(constant (+afmt-mpeg+ "AFMT_MPEG"))

(constant (+afmt-u8+ "AFMT_U8"))
(constant (+afmt-s8+ "AFMT_S8"))

(constant (+afmt-s16-le+ "AFMT_S16_LE"))
(constant (+afmt-s16-be+ "AFMT_S16_BE"))
(constant (+afmt-s16-ne+ "AFMT_S16_NE"))
(constant (+afmt-u16-le+ "AFMT_U16_LE"))
(constant (+afmt-u16-be+ "AFMT_U16_BE"))
(constant (+afmt-u16-ne+ "AFMT_U16_NE"))

(constant (+afmt-s24-le+ "AFMT_S24_LE"))
(constant (+afmt-s24-be+ "AFMT_S24_BE"))
(constant (+afmt-s24-ne+ "AFMT_S24_NE"))
(constant (+afmt-u24-le+ "AFMT_U24_LE"))
(constant (+afmt-u24-be+ "AFMT_U24_BE"))
(constant (+afmt-u24-ne+ "AFMT_U24_NE"))

(constant (+afmt-s32-le+ "AFMT_S32_LE"))
(constant (+afmt-s32-be+ "AFMT_S32_BE"))
(constant (+afmt-s32-ne+ "AFMT_S32_NE"))
(constant (+afmt-u32-le+ "AFMT_U32_LE"))
(constant (+afmt-u32-be+ "AFMT_U32_BE"))
(constant (+afmt-u32-ne+ "AFMT_U32_NE"))
