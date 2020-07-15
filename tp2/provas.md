## Monad LogListDur

$T_A\ X = (A^\star \times (\N \times X))^\star$

$
\eta_X : X \rightarrow T_A\ X\\
\eta_X\ = singl \cdot \langle nil, \langle zero, id \rangle \rangle     $



$
f : X \rightarrow T_A\ Y\\
f^\star : T_A\ X \rightarrow T_A\ Y\\
f^\star = concat \cdot map\ g\\
where\ g\ (l, (t, x)) = map\ (\lambda (l', d) \rightarrow (l ++ l_1, wait_t\ d))\ (f\ x) $

