;CS61A HW10 SICP problems
;Ex 3.38
;a) different final values include: $40, $35, $45
;b) with non-sequential/interleaved operations, other values could be $90, $120, $50 
;if access values are retained to set final values while processes are interleaved.
;
;Ex 3.39
;the only possibilites with the serialized version should be x=100, and x=121
;
;Ex 3.40
;the possible results of (define x 10) (parallel-execute (lambda () (set! x (* x x))) (lambda () (set! x (* x x x))))
;should be x=10^6, x=100, x=1000, x=10000, x=100000, x=10000
;if using serialized procedures, the only possible outcome should be x=10^6

;Ex 3.41
;i believe the change to make the 'balance procedure serialized is unnecessary
;returning the balance to a user has no potential to change the state in any way, and should not affect any
;concurrent operations. However, from a human standpoint, if a change to my account is currently being computed,
;it may affect my financial decision making once the computation is finished.
;Though, it may not be worth the effort for the small chance that I check my balance during the microsecond
;that a change is being processed, as well as cause downtime for a potentially likely expected change in balance,
;such as pay-day or paying rent.

;Ex 3.42
;I believe the reordering of the serialized procedures described in 3.42 will not affect the behavior of the
;account. Defining the protected procedures outside of the dispatch procedure for the account should
;not cause any unintended sharing or separation of mutex between the two procedures.

;3.44
;I believe this issue was discussed in the CS61A SP2010 lecture. The provided procedure in 3.44 does not protect
;if one user is transferring money from account 1 to account 2, while another user is transferring money from
;account 2 to account 1. If both processes are running concurrently, both processes might each grab one of the two mutex.
;This will cause a deadlock, and both processes will not be able to proceed.

;Also, anyone else transferring money between one of the associated accounts may change the state of the accounts
;while one of the account's serializers is waiting to have access to the account.
;For example, if money is being transferred from account 1 to account 2 using the method described in 3.44,
;the (to-account 'deposit) procedure may be waiting on another process while the money has been withdrawn from
;account 1. If something happens to account 2 during this wait, such as the account being closed, moved, or
;structurally changed in some way, the money may be lost during the transaction. A more secure method should be used.

;3.46
;The implemented test-and-set! still has issues. While running test-and-set! for one process and receiving
;the return that the mutex is free, another concurrent process can also be running test-and-set! for the same
;mutex. Multiple concurrent processes can believe they have rights to run processes on the mutex variable.

;3.48
;The change to bank accounts described in 3.48 solves the deadlock problem for exchanges, in which concurrent
;transactions each pull one mutex, and never releasing it, since both concurrent processes have claimed one
;of the needed mutexes. By implementing account numbers, and always taking the lowest account number mutex first,
;despite the direction of money transfer, the deadlock problem is avoided. If both exchanges are ran concurrently,
;both processes will attempt to grab the lower account number mutex, and only one will succeed. The other process
;will then wait while the successful process is completed in entirety.
