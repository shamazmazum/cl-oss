(in-package :oss)

(include "sys/soundcard.h")

;; Audio formats
(bitfield audio-format
  ;; Query
  ((:afmt-query "AFMT_QUERY")
   :documentation "Query current format")
  ;; 8 bits
  ((:afmt-u8 "AFMT_U8")
   :documentation "Unsigned 8-bit samples")
  ((:afmt-s8 "AFMT_S8")
   :documentation "Signed 8-bit samples")
  ;; 16 bits
  ((:afmt-s16-le "AFMT_S16_LE")
   :documentation "Signed 16-bit little endian samples")
  ((:afmt-s16-be "AFMT_S16_BE")
   :documentation "Signed 16-bit bit endian samples")
  ((:afmt-s16-ne "AFMT_S16_NE")
   :documentation "Signed 16-bit native endian samples")
  ((:afmt-u16-le "AFMT_U16_LE")
   :documentation "Unsigned 16-bit little endian samples")
  ((:afmt-u16-be "AFMT_U16_BE")
   :documentation "Unsigned 16-bit bit endian samples")
  ((:afmt-u16-ne "AFMT_U16_NE")
   :documentation "Unsigned 16-bit native endian samples")
  ;; 24 bits (these are optional, because linux does not have them)
  ((:afmt-s24-le "AFMT_S24_LE")
   :documentation "Signed 24-bit little endian samples"
   :optional t)
  ((:afmt-s24-be "AFMT_S24_BE")
   :documentation "Signed 24-bit bit endian samples"
   :optional t)
  ((:afmt-s24-ne "AFMT_S24_NE")
   :documentation "Signed 24-bit native endian samples"
   :optional t)
  ((:afmt-u24-le "AFMT_U24_LE")
   :documentation "Unsigned 24-bit little endian samples"
   :optional t)
  ((:afmt-u24-be "AFMT_U24_BE")
   :documentation "Unsigned 24-bit bit endian samples"
   :optional t)
  ((:afmt-u24-ne "AFMT_U24_NE")
   :documentation "Unsigned 24-bit native endian samples"
   :optional t)
  ;; 32 bits (optional, see above)
  ((:afmt-s32-le "AFMT_S32_LE")
   :documentation "Signed 32-bit little endian samples"
   :optional t)
  ((:afmt-s32-be "AFMT_S32_BE")
   :documentation "Signed 32-bit bit endian samples"
   :optional t)
  ((:afmt-s32-ne "AFMT_S32_NE")
   :documentation "Signed 32-bit native endian samples"
   :optional t)
  ((:afmt-u32-le "AFMT_U32_LE")
   :documentation "Unsigned 32-bit little endian samples"
   :optional t)
  ((:afmt-u32-be "AFMT_U32_BE")
   :documentation "Unsigned 32-bit bit endian samples"
   :optional t)
  ((:afmt-u32-ne "AFMT_U32_NE")
   :documentation "Unsigned 32-bit native endian samples"
   :optional t)
  ;; Compressed formats (make them optional as well)
  ((:afmt-mu-law "AFMT_MU_LAW")
   :documentation "G.771 μ-law format"
   :optional t)
  ((:afmt-a-law "AFMT_A_LAW")
   :documentation "G.771 A-law format"
   :optional t)
  ((:afmt-ima-adpcm "AFMT_IMA_ADPCM")
   :documentation "ADPCM format"
   :optional t)
  ((:afmt-mpeg "AFMT_MPEG")
   :documentation "MPEG MP2/MP3 audio"
   :optional t))

;; ioctl requests
(constant (+sndctl-dsp-setfmt+ "SNDCTL_DSP_SETFMT")
  :documentation "Set audio format")
(constant (+sndctl-dsp-getfmts+ "SNDCTL_DSP_GETFMTS")
  :documentation "Get supported formats")
(constant (+sndctl-dsp-channels+ "SNDCTL_DSP_CHANNELS")
  :documentation "Set number of channels")
(constant (+sndctl-dsp-speed+ "SNDCTL_DSP_SPEED")
  :documentation "Set sample rate")
