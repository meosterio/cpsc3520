/* simple_sab_r2.pro   LGN grammar */
/* {}/1 is LGN 'protect' */

s --> a,{write('now consider subgoal b'),nl},b.
a --> [ta].
b --> [tb].
