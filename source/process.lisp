;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

;;; Copyright (c) 1998-2014, 
;;; Volker Haarslev, Ralf Moeller, Michael Wessel.  
;;; All rights reserved.

;;; Racer is distributed under the following BSD 3-clause license

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are
;;; met:

;;; Redistributions of source code must retain the above copyright notice,
;;; this list of conditions and the following disclaimer.

;;; Redistributions in binary form must reproduce the above copyright
;;; notice, this list of conditions and the following disclaimer in the
;;; documentation and/or other materials provided with the distribution.

;;; Neither the name Racer nor the names of its contributors may be used
;;; to endorse or promote products derived from this software without
;;; specific prior written permission.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
;;; BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
;;; FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
;;; VOLKER HAARSLEV, RALF MOELLER, NOR MICHAEL WESSEL BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES, LOSS OF USE, DATA, OR PROFITS, OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
;;; IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
;;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :thematic-substrate)

(defvar *lock-owned-by* '(?))

(defvar *process-counter* 0)

(defparameter *process-diagnostics-p* nil)

;;;
;;;
;;;

#-:multiprocess-queries
(progn


  (defmacro start-process (&body body)  
    `(progn 
       (funcall #'(lambda ()
                    ,@body))
     
       nil ; wichtig!
       ))

  (defun kill-process (process)
    (declare (ignorable process))
    t)

  (defmacro with-critical-section (&body body)
    `(progn ,@body))

  (defmacro with-access-to-lifecycle-lists (&body body)
    `(progn ,@body))

  (defmacro process-wait (&rest body)
    `(progn ,@body))

  (defun release-locks ()
    )
  
  (defmacro with-process-lock ((&rest args) &body body)
    (declare (ignorable args))
    `(progn 
       ,@body))


  #+ (or allegro sbcl)
  (defun make-lock ()
    (mp:make-process-lock))

  #+:lispworks
  (defun make-lock ()
    (mp:make-lock :important-p nil
                  #+(or :lispworks6 :lispworks7) :safep #+(or :lispworks6 :lispworks7) t
                  #+(or :lispworks6 :lispworks7) :recursivep #+(or :lispworks6 :lispworks7) t
                  ))

  #+:ccl
  (defun make-lock ()
    (ccl:make-lock))

  #+(or :abcl :sbcl)
  (defun make-lock ()
    nil)


  )

;;;
;;;
;;;

#+:multiprocess-queries
(progn 

  #+abcl
  (defmacro start-process (&body body)  
    `(if *multiprocess-queries*
         (to-be-implemented 'start-process)
       (progn 
         (funcall #'(lambda ()
                      ,@body)
                  nil)
         nil)))


  #+:ccl
  (defmacro start-process (&body body)  
    `(if *multiprocess-queries*
         (ccl:process-run-function
                         
          (format nil "Query Answering Process ~A" (incf *process-counter*))
        
          #'(lambda ()
              ,@body))
       (progn 
         (funcall #'(lambda ()
                      ,@body))
         nil)))

  #+ (or allegro sbcl)
  (defmacro start-process (&body body)  
    `(if *multiprocess-queries*
         (mp:process-run-function
          (format nil "Query Answering Process ~A" (incf *process-counter*))
        
          #'(lambda (*standard-output* 
                     *trace-output*
                     *debug-io* 
                     *terminal-io* 
                     *error-output*)
              ,@body)

          *standard-output* 
          *trace-output*
          *debug-io* 
          *terminal-io* 
          *error-output*)
       
       (progn 
         (funcall #'(lambda ()
                      ,@body))
         nil)))


  #+:lispworks
  (defmacro start-process (&body body)  
    `(if *multiprocess-queries*
         (mp:process-run-function 

          (format nil "Query Answering Process ~A" (incf *process-counter*))
           
          nil ;'(:priority 80000000)
          
          #'(lambda (*standard-output* 
                     *trace-output*
                     *debug-io* 
                     *terminal-io* 
                     *error-output*)
              ,@body)
          *standard-output* 
          *trace-output*
          *debug-io* 
          *terminal-io* 
          *error-output*
          )
       
       (progn 
         (funcall #'(lambda ()
                      ,@body))
         nil)))
  
  ;;;
  ;;;
  ;;;

  (defun kill-process (process)
    (declare (ignorable process))
    #+:lispworks
    (mp:process-kill process)
    #+:ccl 
    (ccl:process-kill process)
    #+ (or allegro sbcl) 
    (mp:process-kill process)
    #+abcl
    (to-be-implemented 'kill-process1))

  ;;;
  ;;;
  ;;;

  #+ (or :allegro sbcl)
  (defun make-lock ()
    (mp:make-process-lock))

  #+:lispworks
  (defun make-lock ()
    (mp:make-lock :important-p nil
                  #+(or :lispworks6 :lispworks7) :safep #+(or :lispworks6 :lispworks7) t
                  #+(or :lispworks6 :lispworks7) :recursivep #+(or :lispworks6 :lispworks7) t
                  ))

  #+:ccl
  (defun make-lock ()
    (ccl:make-lock))

  #+abcl
  (defun make-lock ()
    (to-be-implemented 'make-lock))

  ;;;
  ;;;
  ;;;

  #+ (or allegro sbcl)
  (defun process-sleep (time)
    (mp:process-wait-with-timeout
     "wait" time (lambda () nil)))

  #+:lispworks
  (defun process-sleep (time)
    (mp:process-wait-with-timeout "wait" time))

  #+:ccl
  (defun process-sleep (time)
    (ccl:process-wait-with-timeout "wait" time (lambda () nil)))

  #+abcl
  (defun process-sleep (time)
    (declare (ignorable time))
    (to-be-implemented 'process-sleep))


  ;;;
  ;;;
  ;;;
  (defvar *process-lock* (make-lock)) ; f. Racer

  (defvar *lifecycle-lock* (make-lock)) ; f. Lifecycle Lists (*active-queries*, ...) 

  #+:lispworks
  (defmacro with-process-lock ((&optional (lock *process-lock*)) &body body)
    `(mp:with-lock (,lock)
       ,@body))

  #+ (or allegro sbcl)
  (defmacro with-process-lock ((&optional (lock *process-lock*)) &body body)
    `(mp:with-process-lock
      (,lock)
      ,@body))

  #+:ccl
  (defmacro with-process-lock ((&optional (lock *process-lock*)) &body body)
    (let ((l (gensym)))
      `(let ((,l ,lock))
         (unwind-protect
             (progn (ccl:grab-lock ,l)
               ,@body)
           (ccl:release-lock ,l nil)))))

  #+abcl
  (defmacro with-process-lock ((&optional (lock *process-lock*)) &body body)
    (declare (ignorable body lock))
    (to-be-implemented 'with-process-lock))

  ;;;
  ;;;
  ;;;
  
  #+(and :lispworks (not :lispworks5.1) (not (or :lispworks6 :lispworks7)))
  (defun release-locks ()
    (mp:release-lock *process-lock*)
    (mp:release-lock *lifecycle-lock*))

  #+(and :lispworks (or :lispworks5.1 (or :lispworks6 :lispworks7)))
  (defun release-locks ()
    (mp:process-unlock *process-lock* nil)
    (mp:process-unlock *lifecycle-lock* nil))

  #+sbcl ;; from ZASERVE
  (defun release-locks ()
    (bordeaux-threads:release-lock *process-lock*)
    (bordeaux-threads:release-lock *lifecycle-lock*))

  #+allegro
  (defun release-locks ()
    (when (mp:process-lock-locker *process-lock*)
      (mp:process-unlock *process-lock* (mp:process-lock-locker  *process-lock*)))
    (when (mp:process-lock-locker *lifecycle-lock*)
      (mp:process-unlock *lifecycle-lock* (mp:process-lock-locker  *lifecycle-lock*))))

  #+:ccl 
  (defun release-locks ()
    (when (ccl::%%lock-owner *process-lock*)
      (ccl:release-lock *process-lock* nil))
    (when (ccl::%%lock-owner *process-lock*)
      (ccl:release-lock *lifecycle-lock* nil)))

  ;;;
  ;;;
  ;;;

  (defmacro with-critical-section (&body body)
    `(progn 
       
       (when (and *process-diagnostics-p* *running-query*)
         (format t "Query ~A is waiting for lock owned by ~A~%"
                 (iterator-id *running-query*)
                 (first *lock-owned-by*)))
          
       (with-process-lock (*process-lock*)
     
         (progn
           
           (when *process-diagnostics-p*
             (unless *running-query* 
               (setf *lock-owned-by* '(?)))
             
             (when *running-query* 
               (format t "Lock acquired by query ~A~%" (iterator-id *running-query*))
               (setf *lock-owned-by* 
                     (list 
                      (iterator-id *running-query*)
                      ',body))))
           
           ,@body))))

  (defmacro with-access-to-lifecycle-lists (&body body)
    `(with-process-lock (*lifecycle-lock*)
       ,@body))

  ;;;
  ;;;
  ;;;
  
  #+ (or allegro sbcl)
  (defmacro process-wait (&rest body)
    `(mp:process-wait
      "waiting"
      (lambda ()
        ,@body)))

  #+:lispworks
  (defmacro process-wait (&rest body)
    `(mp:process-wait "waiting"
                      (lambda ()
                        ,@body)))

  #+:ccl
  (defmacro process-wait (&rest body)
    `(ccl:process-wait "waiting"
                        (lambda ()
                          ,@body)))

  #+abcl
  (defmacro process-wait (&rest body)
    (declare (ignorable body))
    (to-be-implemented 'process-wait))
  )

;;;
;;;
;;;

(defun kill-current-process ()
  #+:lispworks
  (mp:process-kill mp:*current-process*)
  #+ (or allegro sbcl) 
  (mp:process-kill mp:*current-process*)
  #+:ccl
  (ccl:process-kill ccl:*current-process*)
  #+(or :sbcl :abcl)
  (to-be-implemented 'kill-current-process))


