;;; $Id: gc-bug-test.lisp,v 1.1 2014/05/01 23:53:11 mmondor Exp $
;;;
;;; With the below settings running the test<n> function a few times is
;;; generally required to reproduce the issues.
;;;
;;; Issue #1:
;;; When the OS soft limit for data allocation is reached, expected ENOMEM
;;; errors occur, but ECL seems unable to gracefully report it or silently
;;; continue to work if it can.  One thread enters a busy loop with many
;;; ENOMEM errors.  To reach this condition, the test functions must be used
;;; often enough, succeeding until the process's soft limit is reached.
;;; On NetBSD/amd64, the default for that limit seems to be 256MB with default
;;; ECL settings, and it is controllable via rlimit or sysctl.
;;; It is possible that ECL attempts to report the condition but also requires
;;; some memory to be able to report it.  If this is the case and it could be
;;; adapted such that it can use preallocated/reserved memory to report such
;;; a condition, this might fix this issue.  We should also investigate if a
;;; similar situation occurs when the stack fills, and if so, if a preallocated
;;; stack for this might allow to properly report it too.
;;; Mitigation: This bug does not occur if the user/process datasize
;;; soft/current limit is not reached.  The default heap size of ECL being
;;; 1GB, that soft limit should ideally be over 1GB, or the ECL default heap
;;; reduced.  NetBSD-6/amd64's default soft limit being 256MB, the issue
;;; occurred.
;;;
;;; Issue #2:
;;; When a thread is busy writing using stdio, when libgc goes through the
;;; routine GC_collect_or_expand() -> GC_try_to_collect_inner() ->
;;; GC_stopped_mark() -> GC_stop_world() -> sem_wait() ->
;;; pthread_cond_timedwait(), that stdio thread appears to remain deadlocked
;;; in a busy spinlock, possibly because stdio internally uses locks.
;;; This occurs faster than issue #1, as reaching the soft limit is
;;; unnecessary for this to occur.  If introducing a SLEEP in the writing
;;; loop, reproducing this issue is difficult.
;;; If using stdio with threaded libgc is a known issue, implementing buffered
;;; streams for ECL to replace stdio for common cases should be considered.
;;; There might still remain issues when using stdio FILE for FFI, however.
;;; NOTES:
;;; - I could not reproduce the bug using Linux and the ECL-embedded libgc
;;;   (7.1.9).
;;; - When using the ECL-embedded libgc (7.1.9) instead of the pkgsrc one
;;;   (7.2), performance seemed negatively affected but reproducing this bug
;;;   required longer.
;;; - When avoiding the use of stdio (this actually required writing a custom
;;;   function considering fflush(3) still seemed called using a fifo file
;;;   open using CSTREAM NIL), I could not yet reproduce the issue.
;;;   Update: I think that I tracked the reason for the fflush(3) call to a
;;;   FINISH-OUTPUT call I had forgot in my code; however it's unclear yet why
;;;   ECL would issue fflush(3) on steams with CSTREAM NIL if so.  It was
;;;   still useful to have a custom function, since it seems difficult to get
;;;   ECL to write large buffers rather than a byte at a time despite using
;;;   WRITE-SEQUENCE with the proper output format.
;;; - NetBSD stdio uses rwlocks, and I could reproduce a similar issue using
;;;   ECL rwlocks which are still built over pthread_rwlock_t...
;;;
;;; Issue #3:
;;; Some of the last tests using rwlocks directly produce SIGSEGV.
;;; This was recently discovered, I've not investigated the reason yet.
;;; libgc 7.2e on netbsd-6/amd64, using a close-to-head ECL git checkout.


(defparameter *allocations* 16384)
(defparameter *alloc-size* 4096)
(defparameter *initial-queue-size* 8192)

(defvar *queue* (make-array 1024
			    :adjustable t
			    :fill-pointer 0))



;;; Reproduces issue #1
(defun test1 ()
  (loop
     repeat *allocations*
     do
       (vector-push-extend (make-array *alloc-size*
				       :element-type '(unsigned-byte 8)
				       :adjustable nil
				       :initial-element #x00
				       :fill-pointer *alloc-size*)
			   *queue*
			   0))) ; Grows automatically +1/2 size on ECL



;;; This has the same behaviour as above, issue #1.
(defun test2 ()
  (mp:process-run-function 'test-thread #'test1))



(defun busy-thread ()
  (loop))

;;; Behaves like test1 and test2, yet again only reproducing issue #1.
(defun test3 ()
  (let ((busy-thread (mp:process-run-function 'busy
					      #'busy-thread)))
    (unwind-protect
	 (test1)
      (mp:process-kill busy-thread))))



(defun file-writer-thread ()
  (let ((filename (ext:mkstemp "/tmp/tmp")))
    (unwind-protect
	 (progn
	   (with-open-file (stream filename :direction :output)
	     (loop
		do
		  (write-string "." stream))))
      (delete-file filename))))

;;; Successfully reproduces issue #2.
(defun test4 ()
  (let ((writer-thread (mp:process-run-function 'writer
						#'file-writer-thread)))
    (unwind-protect
	 (test1)
      (mp:process-kill writer-thread))))



;;; The following tests attempt to use rwlocks directly to reproduce bug #2.
;;; Since stdio appears to have issues related to rwlocks with boehm-gc,
;;; and that some of my older code using rwlocks also seemed to have the
;;; same issues, it seemed worth trying.  Success was limited however, and
;;; these might expose other bugs.

(defmacro with-rwlock ((lock op) &body body)
  (assert (member op '(:read :write) :test #'eq))
  (let ((s-lock (gensym)))
    `(let ((,s-lock ,lock))
       (,(if (eq :read op)
             'mp:get-rwlock-read
             'mp:get-rwlock-write) ,s-lock t)
       (unwind-protect
            (progn
              ,@body)
         (,(if (eq :read op)
               'mp:giveup-rwlock-read
               'mp:giveup-rwlock-write) ,s-lock)))))

(defvar *rwlock* (mp:make-rwlock :name 'special-test-rwlock))

(defun read-rwlock-thread ()
  (loop
     do
       (with-rwlock (*rwlock* :read))))

(defun write-rwlock-thread ()
  (loop
     do
       (with-rwlock (*rwlock* :write))))

;;; Did not manage to reproduce.  Read locking without contention.
(defun test5 ()
  (let ((locker-thread (mp:process-run-function 'locker
						#'read-rwlock-thread)))
    (unwind-protect
	 (test1)
      (mp:process-kill locker-thread))))

;;; Did not manage to reproduce.  Write locking without contention.
;;; Produces SIGSEGV after a few tries (reason not yet investigated).
;;; So reproduces bug #3.
(defun test6 ()
  (let ((locker-thread (mp:process-run-function 'locker
						#'write-rwlock-thread)))
    (unwind-protect
	 (test1)
      (mp:process-kill locker-thread))))

;;; Did not manage to reproduce.  Read locking with read locking.
(defun test7 ()
  (let ((locker-thread (mp:process-run-function 'locker
						#'read-rwlock-thread)))
    (unwind-protect
	 (with-rwlock (*rwlock* :read)
	   (test1))
      (mp:process-kill locker-thread))))

;;; Read locking with background write locking.
;;; Produces SIGSEGV after a few tries (reason not yet investigated).
;;; Reproduces bug #3.
(defun test8 ()
  (let ((locker-thread (mp:process-run-function 'locker
						#'write-rwlock-thread)))
    (unwind-protect
	 (with-rwlock (*rwlock* :read)
	   (test1))
      (mp:process-kill locker-thread))))

;;; Write locking with background write locking.
;;; Produces SIGSEGV after a few tries (reason not yet investigated).
;;; Reproduces bug #3.
(defun test9 ()
  (let ((locker-thread (mp:process-run-function 'locker
						#'write-rwlock-thread)))
    (unwind-protect
	 (with-rwlock (*rwlock* :write)
	   (test1))
      (mp:process-kill locker-thread))))

;;; Write locking with background read locking.
;;; Seems to deadlock after a few tries, without busy looping.
(defun test10 ()
  (let ((locker-thread (mp:process-run-function 'locker
						#'read-rwlock-thread)))
    (unwind-protect
	 (with-rwlock (*rwlock* :write)
	   (test1))
      (mp:process-kill locker-thread))))
