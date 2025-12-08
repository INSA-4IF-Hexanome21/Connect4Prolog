:- module(negamax_id, [resolve/3]).
:- use_module(negamax_tt).

% We want high depth because ID allows it
final_depth(8). 

resolve(Board, Token, Scores) :-
    % 1. Clean memory
    negamax_tt:clean_tt,
    final_depth(Max),
    Limit is Max - 1,
    
    % 2. Loop depths 1 to Max-1 to warm up the cache
    forall(between(1, Limit, D),
           (
            % We discard the scores of shallow searches, we only want the TT side-effects
            negamax_tt:solve_internal(Board, Token, D, _)
           )),
           
    % 3. Final Search
    negamax_tt:solve_internal(Board, Token, Max, Scores).