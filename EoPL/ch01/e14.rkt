; This is a proof by induction. We want to prove the correctness
; of partial-vector-sum. 
; Base case: partial-vector-sum with empty vector is zero trivially
; Inductive Hypothesis: Assume partial-vector-sum has the correct
; sum, say k for a vector of length n. We want to show that the
; program will still be correct for a vector of length n+1.
; Suppose we have a vector of length n+1 and we remove the first
; element leaving a vector of length n. We know by the Inductive
; hypothesis that the sum of a vector of length n is k and that
; if we add the beginning element of the vector to k, we obtain
; the correct sum. Hence, our proof is complete