(in-package :oss)

(include "sys/soundcard.h")
(include "unistd.h")
(include "fcntl.h")
(include "sys/ioctl.h")

(define "boolean" "int")

(defwrapper* ("oss_set_fmt" oss-set-fmt) :boolean
  ((fd  :int)
   (fmt :int))
  "int new_fmt = fmt;
   int res;
   res = ioctl (fd, SNDCTL_DSP_SETFMT, &new_fmt);
   if (res == -1) return 0;
   return fmt == new_fmt;")

(defwrapper* ("oss_query_fmt" oss-query-fmt) :int
  ((fd  :int))
  "int fmt = AFMT_QUERY;
   int res;
   res = ioctl (fd, SNDCTL_DSP_SETFMT, &fmt);
   if (res == -1) return 0;
   return fmt;")

(defwrapper* ("oss_get_fmts" oss-get-fmts) :int
  ((fd  :int))
  "int fmt;
   ioctl (fd, SNDCTL_DSP_GETFMTS, &fmt);
   return fmt;")

(defwrapper* ("oss_set_channels" oss-set-channels) :boolean
  ((fd  :int)
   (ch :int))
  "int new_ch = ch;
   int res;
   res = ioctl (fd, SNDCTL_DSP_CHANNELS, &new_ch);
   if (res == -1) return 0;
   return ch == new_ch;")

(defwrapper* ("oss_set_samplerate" oss-set-samplerate) :boolean
  ((fd         :int)
   (samplerate :int))
  "int new_samplerate = samplerate;
   int res;
   res = ioctl (fd, SNDCTL_DSP_SPEED, &new_samplerate);
   if (res == -1) return 0;
   return samplerate == new_samplerate;")
