;;; cuda-mode.el --- NVIDIA CUDA Major Mode derived from C++-mode. -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Jack Morrison

;; Author: Jack Morrison <jackmorrison1@gmail.com>
;; URL: https://github.com/chachi/cuda-mode
;; Keywords: c, languages, cuda
;; Version: 0.1

;; Package-Requires: ((compat "29"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Originally found on EmacsWiki @ http://www.emacswiki.org/emacs/CudaMode

;;; Code:

(require 'compat)

(require 'cc-mode)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts)
  (c-add-language 'cuda-mode 'c++-mode))

;; The `cc-langs.el` file is a good reference for how these constants
;; should be used.

(c-lang-defconst c-primitive-type-kwds
  "Primitive type keywords.  As opposed to the other keyword lists, the
keywords listed here are fontified with the type face instead of the
keyword face.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled.

Do not try to modify this list for end user customizations; the
`*-font-lock-extra-types' variable, where `*' is the mode prefix, is
the appropriate place for that."
  cuda
  (append
   '("dim3"
     "char1" "uchar1" "char2" "uchar2" "char3" "uchar3" "char4" "uchar4"
     "short1" "ushort1" "short2" "ushort2" "short3" "ushort3" "short4" "ushort4"
     "int1" "uint1" "int2" "uint2" "int3" "uint3" "int4" "uint4"
     "long1" "ulong1" "long2" "ulong2" "long3" "ulong3" "long4" "ulong4"
     "longlong1" "ulonglong1" "longlong2" "ulonglong2" "longlong3" "ulonglong3" "longlong4" "ulonglong4"
     "float1" "float2"  "float3" "float4"
     "double1" "double2" "double3" "double4" )
   (c-lang-const c-primitive-type-kwds c++)))

;; These don't get highlighted, why?
(c-lang-defconst c-type-modifier-with-parens-kwds
  cuda (append
	'("__launch_bounds__" "__cluster_dims__" "__maxnreg__")
	(c-lang-const c-type-modifier-with-parens-kwds c++)))

(c-lang-defconst c-paren-nontype-kwds
  cuda (append
	'("__align__")
	(c-lang-const c-paren-nontype-kwds c++)))

(c-lang-defconst c-modifier-kwds
  cuda (append
	'("__host__"
          "__device__"
          "__global__"
          "__constant__"
          "__shared__"
          "__grid_constant__"
          "__managed__"
          "__restrict__"
          "__noinline__"
          "__forceinline__"
          "__inline_hint__"
          )
	(c-lang-const c-modifier-kwds c++)))

(c-lang-defconst c-other-op-syntax-tokens
  "List of the tokens made up of characters in the punctuation or
parenthesis syntax classes that have uses other than as expression
operators."
  cuda (append
	'("<<<" ">>>")
	(c-lang-const c-other-op-syntax-tokens c++)))

(c-lang-defconst c-primary-expr-kwds
  "Keywords besides constants and operators that start primary expressions."
  cuda  '("gridDim" "blockIdx" "blockDim" "threadIdx" "warpSize"))

(eval-and-compile ;; required by cc-mode
  (defvar cuda-builtins
    '(
      ; assertions
      "__assertfail"
      "__assert_fail"
      ; atomics
      "atomicAdd"
      "atomicAnd"
      "atomicCAS"
      "atomicDec"
      "atomicExch"
      "atomicInc"
      "atomicMax"
      "atomicMin"
      "atomicOr"
      "atomicSub"
      "atomicXor"
      "atomicAdd_block"
      "atomicAnd_block"
      "atomicCAS_block"
      "atomicDec_block"
      "atomicExch_block"
      "atomicInc_block"
      "atomicMax_block"
      "atomicMin_block"
      "atomicOr_block"
      "atomicSub_block"
      "atomicXor_block"
      "atomicAdd_system"
      "atomicAnd_system"
      "atomicCAS_system"
      "atomicDec_system"
      "atomicExch_system"
      "atomicInc_system"
      "atomicMax_system"
      "atomicMin_system"
      "atomicOr_system"
      "atomicSub_system"
      "atomicXor_system"
      ; memory fence
      "__threadfence"
      "__threadfence_block"
      "__threadfence_system"
      ; synchronisation
      "__syncthreads"
      "__syncthreads_count"
      "__syncthreads_and"
      "__syncthreads_or"
      "__syncwarp"
      ; integer intrinsics
      "__brev"
      "__brevll"
      "__byte_perm"
      "__clz"
      "__clzll"
      "__dp2a_hi"
      "__dp2a_lo"
      "__dp4a"
      "__ffs"
      "__ffsll"
      "__fns"
      "__funnelshift_l"
      "__funnelshift_lc"
      "__funnelshift_r"
      "__funnelshift_rc"
      "__hadd"
      "__mul24"
      "__mul64hi"
      "__mulhi"
      "__nv_bswap16"
      "__nv_bswap32"
      "__nv_bswap64"
      "__popc"
      "__popcll"
      "__rhadd"
      "__sad"
      "__uhadd"
      "__umul24"
      "__umul64hi"
      "__umulhi"
      "__urhadd"
      "__usad"
      ; load primitives
      "__ldg"
      "__ldcg"
      "__ldca"
      "__ldcs"
      "__ldlu"
      "__ldcv"
      "__stwb"
      "__stcg"
      "__stcs"
      "__stwt"
      ; hints
      "__assume"
      "__builtin_assume"
      "__builtin_assume_aligned"
      "__builtin_expect"
      "__builtin_unreachable"
      ; warp-level primitives
      "__all_sync"
      "__any_sync"
      "__ballot_sync"
      "__activemask"
      "__match_any_sync"
      "__match_all_sync"
      "__reduce_add_sync"
      "__reduce_min_sync"
      "__reduce_max_sync"
      "__reduce_add_sync"
      "__reduce_min_sync"
      "__reduce_max_sync"
      "__reduce_and_sync"
      "__reduce_or_sync"
      "__reduce_xor_sync"
      "__shfl_sync"
      "__shfl_up_sync"
      "__shfl_down_sync"
      "__shfl_xor_sync"
      ; misc
      "__nanosleep"
      "__trap"
      "__brkpt"
      "__prof_trigger"
     )
    "Names of built-in CUDA functions."))

(c-lang-defconst c-other-kwds
  cuda `,(append
	  (c-lang-const c-other-kwds c++)
	  cuda-builtins))

(defconst cuda-font-lock-keywords-1
  (c-lang-const c-matchers-1 cuda)
  "Minimal highlighting for CUDA mode.")

(defconst cuda-font-lock-keywords-2
  (c-lang-const c-matchers-2 cuda)
  "Fast normal highlighting for CUDA mode.")

(defconst cuda-font-lock-keywords-3
  (c-lang-const c-matchers-3 cuda)
  "Accurate normal highlighting for CUDA mode.")

(defvar cuda-font-lock-keywords cuda-font-lock-keywords-3
  "Default expressions to highlight in CUDA mode.")

(defvar cuda-mode-syntax-table nil
  "Syntax table used in cuda-mode buffers.")
(or cuda-mode-syntax-table
    (setq cuda-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table cuda))))

(defvar-keymap cuda-mode-map
  :parent c++-mode-map
  :doc "CUDA keymap inherited from C++")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cu[h]?\\'" . cuda-mode))

;;;###autoload
(define-derived-mode cuda-mode c++-mode "Cuda"
  "Major mode for editing Cuda code.
This mode derives from C++ mode.
Key bindings:
\\{cuda-mode-map}"
  (c-initialize-cc-mode t)
  (c-init-language-vars cuda-mode)
  (c-common-init 'cuda-mode)
  (setq c-buffer-is-cc-mode 'c++-mode)
  (when (boundp 'cc-imenu-c++-generic-expression)
    (c-make-init-lang-vars-fun 'cuda)
    (cc-imenu-init cc-imenu-c++-generic-expression))
  (c-run-mode-hooks 'c-mode-common-hook))

(provide 'cuda-mode)
;;; cuda-mode.el ends here
