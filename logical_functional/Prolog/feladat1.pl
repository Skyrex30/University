sum_list([], 0).
sum_list([F | M], Sum) :-
    sum_list(M, MSum),
    Sum is F + MSum.
