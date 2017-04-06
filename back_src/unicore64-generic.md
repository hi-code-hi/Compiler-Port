;; Generic UNICORE64 Pipeline Description
;; Copyright (C) 2003, 2007, 2008 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.  */

;; Star: This file is a very simple unicore64 pipeline model
;; please do read unicore64 ISA to implement other stages
;; Last modified by Boswell Young


;; Define automaton(s)
;; UNICORE64 

(define_automaton "unicore64")


;; Define cpu unit(s)
;;

; units for eight pipeline.
; DEC, ISS, EXE1, EXE2, MEM, WB 
(define_cpu_unit "iss" "unicore64")
(define_cpu_unit "exe1" "unicore64")
(define_cpu_unit "exe2" "unicore64")
(define_cpu_unit "asr" "unicore64")

; units for register file.
; REGPORTA, REGPORTB 
(define_cpu_unit "regporta" "unicore64")
(define_cpu_unit "regportc" "unicore64")


;; Define reservation for integer instructions
;; 

; SINGLE: pipeline for standard single cycle instructions
; NO extral BYPASS.
(define_insn_reservation "single" 1
  (and (eq_attr "generic_sched" "yes")
       (eq_attr "core_cycles" "single"))
  "iss, exe1, exe2")

; LOAD: pipeline for load or multi-load instructions.
; because we cannot recognize multi-load instructions from instruction type,
; we consider multi-load(1,2,3,4) as the same LOAD reservation.
; We use BYPASS for real latency description. 

; FIXME: this is too simple to describle load/ldm for unicore64:
;   (1) ldm can not expressed; 
;   (2) load instructions with shift operand 3 can not expressed. 


; MULT: pipeline for multiply instrucitons.
; there are three multiply instructions in unicore: MULT, MLA, MLAL. 
; We use BYPASS for real latency description. 

(define_insn_reservation "mult" 4
  (and (eq_attr "generic_sched" "yes")
       (eq_attr "type" "mult"))
  "iss+regporta, exe1, exe2")

(define_insn_reservation "mla" 4
  (and (eq_attr "generic_sched" "yes")
       (eq_attr "type" "mla"))
  "iss+regporta+regportc, exe1, exe2")

(define_insn_reservation "mlal" 4
  (and (eq_attr "generic_sched" "yes")
       (eq_attr "type" "mlal"))
  "iss+regportc, exe1+regportc, exe2")

; BRANCH: pipeline for branch instrucitons.
(define_insn_reservation "branch" 4
  (and (eq_attr "generic_sched" "yes")
       (eq_attr "type" "branch"))
  "asr*2, iss+regporta, exe1, exe2")

