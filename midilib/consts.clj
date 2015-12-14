(ns midilib)

;; Standard MIDI File meta event defs.
(def META-EVENT 0xff)
(def META-SEQ-NUM 0x00)
(def META-TEXT 0x01)
(def META-COPYRIGHT 0x02)
(def META-SEQ-NAME 0x03)
(def META-INSTRUMENT 0x04)
(def META-LYRIC 0x05)
(def META-MARKER 0x06)
(def META-CUE 0x07)
(def META-TRACK-END 0x2f)
(def META-SET-TEMPO 0x51)
(def META-SMPTE 0x53)
(def META-TIME-SIG 0x58)
(def META-KEY-SIG 0x59)
(def META-SEQ-SPECIF 0x7f)


;; Number of MIDI channels
(def MIDI-CHANNELS 16)
;; Number of note per MIDI channel
(def NOTES-PER-CHANNEL 128)

;; Channel messages
;; Note, val
(def NOTE-OFF 0x80)
;; Note, val
(def NOTE-ON 0x90)
;; Note, val
(def POLY-PRESSURE 0xA0)
;; Controller ;, val
(def CONTROLLER 0xB0)
;; Program number
(def PROGRAM-CHANGE 0xC0)
;; Channel pressure
(def CHANNEL-PRESSURE 0xD0)
;; LSB, MSB
(def PITCH-BEND 0xE0)

;; System common messages
;; System exclusive start
(def SYSEX 0xF0)
;; Beats from top: LSB/MSB 6 ticks = 1 beat
(def SONG-POINTER 0xF2)
;; Val = number of song
(def SONG-SELECT 0xF3)
;; Tune request
(def TUNE-REQUEST 0xF6)
;; End of system exclusive
(def EOX 0xF7)

;; System realtime messages
;; MIDI clock (24 per quarter note)
(def CLOCK 0xF8)
;; Sequence start
(def START 0xFA)
;; Sequence continue
(def CONTINUE 0xFB)
;; Sequence stop
(def STOP 0xFC)
;; Active sensing (sent every 300 ms when nothing else being sent)
(def ACTIVE-SENSE 0xFE)
;; System reset
(def SYSTEM-RESET 0xFF)

;;
;; Controller numbers
;; = 0 - 31 = continuous, LSB
;; = 32 - 63 = continuous, MSB
;; = 64 - 97 = switches
;;
(def CC-MOD-WHEEL 1)
(def CC-BREATH-CONTROLLER 2)
(def CC-FOOT-CONTROLLER 4)
(def CC-PORTAMENTO-TIME 5)
(def CC-DATA-ENTRY-MSB 6)
(def CC-VOLUME 7)
(def CC-BALANCE 8)
(def CC-PAN 10)
(def CC-EXPRESSION-CONTROLLER 11)
(def CC-GEN-PURPOSE-1 16)
(def CC-GEN-PURPOSE-2 17)
(def CC-GEN-PURPOSE-3 18)
(def CC-GEN-PURPOSE-4 19)
;; [32 - 63] are LSB for [0 - 31]
;; Momentaries:
(def CC-SUSTAIN 64)
(def CC-PORTAMENTO 65)
(def CC-SUSTENUTO 66)
(def CC-SOFT-PEDAL 67)
(def CC-HOLD-2 69)
(def CC-GEN-PURPOSE-5 50)
(def CC-GEN-PURPOSE-6 51)
(def CC-GEN-PURPOSE-7 52)
(def CC-GEN-PURPOSE-8 53)
(def CC-TREMELO-DEPTH 92)
(def CC-CHORUS-DEPTH 93)
(def CC-DETUNE-DEPTH 94)
(def CC-PHASER-DEPTH 95)
(def CC-DATA-INCREMENT 96)
(def CC-DATA-DECREMENT 97)
(def CC-NREG-PARAM-LSB 98)
(def CC-NREG-PARAM-MSB 99)
(def CC-REG-PARAM-LSB 100)
(def CC-REG-PARAM-MSB 101)

;; Channel mode message values
;; Val 0 == off, 0x7f == on
(def CM-LOCAL-CONTROL 0x7A)
(def CM-ALL-NOTES-OFF 0x7B)             ; Val must be 0
(def CM-OMNI-MODE-OFF 0x7C)             ; Val must be 0
(def CM-OMNI-MODE-ON 0x7D)              ; Val must be 0
(def CM-MONO-MODE-ON 0x7E)              ; Val = number of chans
(def CM-POLY-MODE-ON 0x7F)              ; Val must be 0
