;;;-*-Mode: LISP; Package: CCL -*-
;;
;; KQUEUE.LISP
;;
;; KQUEUE - BSD kernel event notification mechanism support for Common LISP.
;; Copyright (C) 2007 Terje Norderhaug <terje@in-progress.com>
;; Released under LGPL - see <http://www.gnu.org>.
;; Alternative licensing available upon request.
;; 
;; DISCLAIMER: The user of this module should understand that executing code is a potentially hazardous 
;; activity, and that many dangers and obstacles, marked or unmarked, may exist within this code.
;; As a condition of your use of the module, you assume all risk of personal injury, death, or property
;; loss, and all other bad things that may happen, even if caused by negligence, ignorance or stupidity.
;; The author is is no way responsible, and besides, does not have "deep pockets" nor any spare change.
;;
;; Version: 0.20 alpha (July 26, 2009) - subject to major revisions, so consider yourself warned.
;; Tested with Macintosh Common LISP 5.1 and 5.2, but is intended to be platform and system independent in the future.
;;
;; Email feedback and improvements to <terje@in-progress.com>.
;; Updated versions will be available from <http://www.in-progress.com/src/>.
;;
;; RELATED IMPLEMENTATIONS
;; There is another kevent.lisp for other platforms by Risto Laakso (merge?).
;; Also a Scheme kevent.ss by Jose Antonio Ortega.
;;
;; SEE ALSO:
;; http://people.freebsd.org/~jlemon/papers/kqueue.pdf
;; http://developer.apple.com/samplecode/FileNotification/index.html
;; The Man page for kqueue() or kevent().
;; PyKQueue - Python OO interface to KQueue.
;; LibEvent - an event notification library in C by Niels Provos.
;; Liboop - another abstract library in C on top of kevent or other kernel notification.

#| HISTORY:

2007-Oct-18 terje version 0.1 released on the Info-MCL mailing list.
2008-Aug-21 terje load-framework-bundle is not needed under MCL 5.2
2008-Aug-21 terje rename get-addr to lookup-function-in-bundle (only for pre MCL 5.2)
2009-Jul-19 terje uses kevent-error condition and strerror.
2009-Jul-24 terje reports errors unless nil-if-not-found in lookup-function-in-bundle. 
2009-Jul-24 terje kevent :variant for C's intptr_t type for 64bit (and osx 10.5) compatibility.
2009-Jul-25 terje 64bit support, dynamically determined for PPC. Kudos to Glen Foy for helping out.
2009-Jul-25 terje make-kevent function.
|#

#| IMPLEMENTATION NOTES:

kevents are copied into and from the kernel, so the records don't have to be kept in the app!
kevents does not work in OSX before 10.3.
*kevent-record* has to be explcitly set to :kevent64 to work on 64bit intel macs.
Consider using sysctlbyname() to test for 64bit, 
 combining hw.cpu64bit_capable, hw.optional.x86_64 and hw.optional.64bitops
|#

(in-package :ccl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-ccl-5.2 ; has been added to MCL 5.2
(defmethod load-framework-bundle ((framework-name string) &key (load-executable t))
  ;; FRAMWORK CALL FUNCTIONALITY FROM BSD.LISP
  ;; (C) 2003 Brendan Burns <bburns@cs.umass.edu>
  ;; Released under LGPL.
  (with-cfstrs ((framework framework-name))
    (let ((err 0)
          (baseURL nil)
          (bundleURL nil)
          (result nil))
      (rlet ((folder :fsref))
        ;; Find the folder holding the bundle
        (setf err (#_FSFindFolder #$kOnAppropriateDisk #$kFrameworksFolderType 
                   t folder))
        
        ;; if everything's cool, make a URL for it
        (when (zerop err)
          (setf baseURL (#_CFURLCreateFromFSRef (%null-ptr) folder))
          (if (%null-ptr-p baseURL) 
            (setf err #$coreFoundationUnknownErr)))
        
        ;; if everything's cool, make a URL for the bundle
        (when (zerop err)
          (setf bundleURL (#_CFURLCreateCopyAppendingPathComponent (%null-ptr) 
                           baseURL framework nil))
          (if (%null-ptr-p bundleURL) 
            (setf err #$coreFoundationUnknownErr)))
        
        ;; if everything's cool, load it
        (when (zerop err)
          (setf result (#_CFBundleCreate (%null-ptr) bundleURL))
          (if (%null-ptr-p result)
            (setf err #$coreFoundationUnknownErr)))
        
        ;; if everything's cool, and the user wants it loaded, load it
        (when (and load-executable (zerop err))
          (if (not (#_CFBundleLoadExecutable result))
            (setf err #$coreFoundationUnknownErr)))
        
        ;; if there's an error, but we've got a pointer, free it and clear result
        (when (and (not (zerop err)) (not (%null-ptr-p result)))
          (#_CFRelease result)
          (setf result nil))
        
        ;; free the URLs if there non-null
        (when (not (%null-ptr-p bundleURL))
          (#_CFRelease bundleURL))
        (when (not (%null-ptr-p baseURL))
          (#_CFRelease baseURL))
        
        ;; return pointer + error value
        (values result err)))))

#+ignore
(defun get-addr (bundle name)
  (let* ((addr (#_CFBundleGetFunctionPointerForName bundle name)))
    (rlet ((buf :long))
      (setf (%get-ptr buf) addr)
      (ash (%get-signed-long buf) -2))))

#-ccl-5.2
(defun lookup-function-in-bundle (name bundle &optional nil-if-not-found)
  (with-cfstrs ((str name))
    (let* ((addr (#_CFBundleGetFunctionPointerForName bundle str)))
      (if (%null-ptr-p addr)
        (unless nil-if-not-found
          (error "Couldn't resolve address of foreign function ~s" name))
        (rlet ((buf :long)) ;; mcl 5.2 uses %fixnum-from-macptr here
          (setf (%get-ptr buf) addr)
          (ash (%get-signed-long buf) -2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenient way to declare BSD system calls

#+ignore
(defparameter *system-bundle*
  #+ccl-5.2 (get-bundle-for-framework-name "System.framework")
  #-ccl-5.2
  (let ((bundle (load-framework-bundle "System.framework")))
    (terminate-when-unreachable bundle (lambda (b)(#_CFRelease b)))
    bundle))

(defmacro declare-bundle-ff (name name-string &rest arglist &aux (fn (gensym (format nil "ff_~A_" (string name)))))
  ;; Is there an existing define-trap like macro for this? or could one be modified for use with bundles?
  `(progn
     (defloadvar ,fn
       (let* ((bundle #+ccl-5.2 (get-bundle-for-framework-name "System.framework")
                      #-ccl-5.2
                      (let ((bundle (load-framework-bundle "System.framework")))
                        (terminate-when-unreachable bundle (lambda (b)(#_CFRelease b)))
                        bundle)))
         (lookup-function-in-bundle ,name-string bundle)))
     ,(let ((args (do ((arglist arglist (cddr arglist))
                      (result))
                     ((not (cdr arglist)) (nreverse result))
                   (push (second arglist) result))))        
       `(defun ,name ,args
          (ppc-ff-call ,fn ,@arglist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare-bundle-ff %system-kqueue "kqueue" 
                   :signed-fullword) ;; returns a file descriptor no!

(defun system-kqueue ()
  (let ((kq (%system-kqueue)))
    (if (= kq -1)
      (ecase (%system-errno)
        (12 (error "The kernel failed to allocate enough memory for the kernel queue")) ; ENOMEM
        (24 (error "The per-process descriptor table is full")) ; EMFILE
        (23 (error "The system file table is full"))) ; ENFILE 
      kq)))

(declare-bundle-ff %system-kevent "kevent"
                  :unsigned-fullword kq
                  :address ke
                  :unsigned-fullword nke
                  :address ko
                  :unsigned-fullword nko
                  :address timeout
                  :signed-fullword)

(declare-bundle-ff %system-open "open" 
                   :address name 
                   :unsigned-fullword mode
                   :unsigned-fullword arg 
                   :signed-fullword)
 
(declare-bundle-ff %system-close "close"
                   :unsigned-fullword fd 
                   :signed-fullword)

(declare-bundle-ff %system-errno* "__error" 
                   :signed-fullword)

(declare-bundle-ff %system-strerror "strerror" 
                   :signed-fullword errno
                   :address)

(defun %system-errno ()
  (%get-fixnum (%int-to-ptr (%system-errno*))))

; (%system-errno)

(defconstant $O-EVTONLY #x8000)
; (defconstant $O-NONBLOCK #x800 "Non blocking mode")

(defun system-open (posix-namestring)
  "Low level open function, as in C, returns an fd number"
  (with-cstrs ((name posix-namestring))
    (%system-open name $O-EVTONLY 0)))

(defun system-close (fd)
  (%system-close fd))

(defrecord timespec
  (sec :unsigned-long)
  (usec :unsigned-long))

(defVar *kevent-record* nil)

(def-ccl-pointers determine-64bit-kevents ()
  (setf *kevent-record*
       (if (ccl::gestalt #$gestaltPowerPCProcessorFeatures
                        #+ccl-5.2 #$gestaltPowerPCHas64BitSupport #-ccl-5.2 6)
          :kevent32
          :kevent64)))

(defrecord :kevent32
  (ident :unsigned-long) ; uintptr_t
  (filter :short)
  (flags :unsigned-short)
  (fflags :unsigned-long)
  (data :long)  ; intptr_t
  (udata :pointer))

(defrecord :kevent64
  (:variant ; uintptr_t
   ((ident64 :uint64))
   ((ident :unsigned-long)))
  (filter :short)
  (flags :unsigned-short)
  (fflags :unsigned-long)
  (:variant  ; intptr_t
   ((data64 :sint64))
   ((data :long)))
  (:variant ; RMCL :pointer is 32bit
   ((udata64 :uint64))
   ((udata :pointer))))

(defun make-kevent (&key (ident 0) (filter 0) (flags 0) (fflags 0) (data 0) (udata *null-ptr*))
   (ecase *kevent-record*
      (:kevent64   
       (make-record kevent64
                    :ident ident
                    :filter filter 
                    :flags flags
                    :fflags fflags
                    :data data 
                    :udata udata))
      (:kevent32
       (make-record kevent32
                    :ident ident
                    :filter filter 
                    :flags flags
                    :fflags fflags
                    :data data 
                    :udata udata))))

(defun kevent-rref (ke field)
   (ecase *kevent-record*
      (:kevent32
       (ecase field
          (:ident (rref ke :kevent32.ident))
          (:filter (rref ke :kevent32.filter))
          (:flags (rref ke :kevent32.flags))
          (:fflags (rref ke :kevent32.fflags))
          (:data (rref ke :kevent32.data))
          (:udata (rref ke :kevent32.udata))))
      (:kevent64
       (ecase field
          (:ident (rref ke :kevent64.ident))
          (:filter (rref ke :kevent64.filter))
          (:flags (rref ke :kevent64.flags))
          (:fflags (rref ke :kevent64.fflags))
          (:data (rref ke :kevent64.data))
          (:udata (rref ke :kevent64.udata))))))

(defun kevent-filter (ke)
   (kevent-rref ke :filter))

(defun kevent-flags (ke)
   (kevent-rref ke :flags))

(defun kevent-data (ke)
   (kevent-rref ke :data))


;; FILTER TYPES:

(eval-when (:compile-toplevel :load-toplevel :execute) ; added by binghe

(defconstant $kevent-read-filter -1 "Data available to read")
(defconstant $kevent-write-filter -2 "Writing is possible")
(defconstant $kevent-aio-filter -3 "AIO system call has been made")
(defconstant $kevent-vnode-filter -4 "Event occured on a file descriptor")
(defconstant $kevent-proc-filter -5 "Process performed one or more of the requested events")
(defconstant $kevent-signal-filter -6 "Attempted to deliver a signal to a process")
(defconstant $kevent-timer-filter -7 "Establishes an arbitrary timer")
(defconstant $kevent-netdev-filter -8 "Event occured on a network device")
(defconstant $kevent-filesystem-filter -9)

) ; eval-when

; FLAGS:

(defconstant $kevent-add #x01)
(defconstant $kevent-delete #x02)
(defconstant $kevent-enable #x04)
(defconstant $kevent-disable #x08)
(defconstant $kevent-oneshot #x10)
(defconstant $kevent-clear #x20)
(defconstant $kevent-error #x4000)
(defconstant $kevent-eof #x8000 "EV_EOF")

;; FFLAGS:

(defconstant $kevent-file-delete #x01 "The file was unlinked from the file system")
(defconstant $kevent-file-write #x02 "A write occurred on the file")
(defconstant $kevent-file-extend #x04 "The file was extended")
(defconstant $kevent-file-attrib #x08 "The file had its attributes changed")
(defconstant $kevent-file-link #x10 "The link count on the file changed")
(defconstant $kevent-file-rename #x20 "The file was renamed")
(defconstant $kevent-file-revoke #x40 "Access to the file was revoked or the file system was unmounted")
(defconstant $kevent-file-all (logior $kevent-file-delete $kevent-file-write $kevent-file-extend
                                      $kevent-file-attrib $kevent-file-link $kevent-file-rename $kevent-file-revoke))


(defconstant $kevent-net-linkup #x01 "Link is up")
(defconstant $kevent-net-linkdown #x02 "Link is down")
(defconstant $kevent-net-linkinvalid #x04 "Link state is invalid")
(defconstant $kevent-net-added #x08 "IP adress added")
(defconstant $kevent-net-deleted #x10 "IP adress deleted")

(define-condition kevent-error (simple-error)
  ((errno :initform NIL :initarg :errno)
   (ko :initform nil :type (or null kevent) :initarg :ko)
   (syserr :initform (%system-errno)))
  (:report 
   (lambda (c s)
     (with-slots (errno ko syserr) c
       (format s "kevent system call error ~A [~A]" errno syserr) 
       (when errno 
          (format s "(~A)" (%get-cstring (%system-strerror errno))))
       (when ko
          (format s " for ")
          (let ((*standard-output* s))
            (print-record ko *kevent-record*)))))))

(defun %kevent (kq &optional ke ko (timeout 0))
  (check-type kq integer)
  (rlet ((&timeout :timespec :sec timeout :usec 1))
    (let ((num (with-timer ;; does not seem to make a difference...  
                 (%system-kevent kq (or ke (%null-ptr))(if ke 1 0)(or ko (%null-ptr))(if ko 1 0) &timeout))))
      ; "If an error occurs while processing an element of the changelist and there 
      ; is enough room in the eventlist, then the event will be placed in the eventlist with 
      ; EV_ERROR set in flags and the system error in data."
      (when (and ko (plusp (logand $kevent-error (kevent-flags ko))))
        (error 'kevent-error 
                              :errno (kevent-data ko)
               :ko ko))
      ; "Otherwise, -1 will be returned, and errno will be set to indicate the error condition."
      (when (= num -1)
        ;; hack - opentransport provides the constants for the errors documented for the call 
        (case (%system-errno)
          (0 (error "kevent system call failed with an unspecified error")) ;; should not happen!
          (13 (error "The process does not have permission to register a filter")) 
          (14 (error "There was an error reading or writing the kevent structure"))  ; EFAULT
          (9 (error "The specified descriptor is invalid")) ; EBADF
          (4 (error "A signal was delivered before the timeout expired and before any events were placed on the kqueue for return.")) ; EINTR
          (22 (error "The specified time limit or filter is invalid")) ; EINVAL
          (2 (error "The event could not be found to be modified or deleted")) ; ENOENT
          (12 (error "No memory was available to register the event")) ; ENOMEM
          (78 (error "The specified process to attach to does not exist"))) ; ESRCH
        ;; shouldn't get here... 
        (errchk (%system-errno))
        (error "error ~A" (%system-errno)))
      (unless (zerop num)
         (values ko num)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOS INTERFACE

(defclass kqueue ()
  ((kq :initform (system-kqueue) 
       :documentation "file descriptor referencing the kqueue")
   (fds :initform NIL)) ;; ## better if kept on top level, perhaps as a hash table...
  (:documentation "A kernal event notification channel"))

(defmethod initialize-instance :after ((q kqueue) &rest rest)
  (declare (ignore rest))
  (terminate-when-unreachable q 'kqueue-close))

(defmethod kqueue-close ((q kqueue))
  (with-slots (kq fds) q
    (when (or kq fds) ;; allow repeated close
      (system-close kq)
      (setf fds NIL)
      (setf kq NIL))))

(defmethod kqueue-poll ((q kqueue))
  "Polls a kqueue for kevents"
  ;; may not have to be cleared, but just in case:
  (flet ((kqueue-poll2 (ko)
           (let ((result (with-slots (kq) q
                            (without-interrupts 
                             (%kevent kq NIL ko)))))
             (when result
                (let ((type  (kevent-filter result)))
                  (ecase type
                     (0 (values))
                     (#.$kevent-read-filter
                          (values
                           :read
                           (kevent-rref result :ident)
                           (kevent-rref result :flags)
                           (kevent-rref result :fflags)
                           (kevent-rref result :data)
                           (kevent-rref result :udata)))
                      (#.$kevent-write-filter :write)
                      (#.$kevent-aio-filter :aio)
                      (#.$kevent-vnode-filter
                           (values
                            :vnode
                            (cdr (assoc (kevent-rref result :ident) (slot-value q 'fds)))
                            (kevent-rref result :flags)
                            (kevent-rref result :fflags)
                            (kevent-rref result :data)
                            (kevent-rref result :udata)))
                      (#.$kevent-filesystem-filter :filesystem)))))))
    (ecase *kevent-record*
       (:kevent64
        (rlet ((ko :kevent64 :ident 0 :filter 0 :flags 0 :fflags 0 :data 0 :udata (%null-ptr)))
          (kqueue-poll2 ko)))
       (:kevent32
        (rlet ((ko :kevent32 :ident 0 :filter 0 :flags 0 :fflags 0 :data 0 :udata (%null-ptr)))
          (kqueue-poll2 ko))))))

(defmethod kqueue-subscribe ((q kqueue) &key ident filter (flags 0) (fflags 0) (data 0) (udata (%null-ptr)))
  (let ((ke (make-kevent :ident ident
                         :filter filter 
                         :flags flags
                         :fflags fflags
                         :data data 
                         :udata udata)))
    (with-slots (kq) q
       (without-interrupts
        (%kevent kq ke)))))

(defmethod kqueue-vnode-subscribe ((q kqueue) pathname)
  "Makes the queue report an event when there is a change to a directory or file" 
  (let* ((namestring (posix-namestring (full-pathname pathname)))
         (fd (system-open namestring)))
    (with-slots (fds) q
      (push (cons fd pathname) fds))
    (kqueue-subscribe q 
                      :ident fd 
                      :filter $kevent-vnode-filter 
                      :flags (logior $kevent-add $kevent-clear) 
                      :fflags $kevent-file-all)
    namestring))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+test
(defun kevent-d (pathname &optional (*standard-output* (fred)))
  "Report changes to a file or directory"
  (loop
    with kqueue = (make-instance 'kqueue)
    with sub = (kqueue-vnode-subscribe kqueue pathname) 
    for i from 1 to 60
    for result = (multiple-value-list (kqueue-poll kqueue))
    unless (equal result '(NIL))
    do (progn
         (format T "~A~%" result)
         (force-output))
    ; do (process-allow-schedule)
    do (sleep 1)
    finally (write-line "Done")
    ))

#|

; Report changes to this file in a fred window (save this document to see what happens):

(process-run-function "kevent-d" #'kevent-d *loading-file-source-file*
                      (fred))

; Reports files added or removed from the directory of this file:

(process-run-function "kevent-d" #'kevent-d 
                      (make-pathname :directory (pathname-directory *loading-file-source-file*))
                      (fred))
|#



