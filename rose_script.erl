-module(rose_script).
-export([eval/1]).

-include("rose_opcodes.hrl").

eval(Script) ->
    eval(Script, {[], []}).

eval(<<H:8, T/binary>>, Stacks) ->
    eval(T, eval_code(H, Stacks));
eval(<<>>, Stacks) ->
    Stacks.

eval_code(Op, {S1, S2}) ->
    case Op of
        1 ->
            {[1 | S1], S2};
        ?FALSE ->
            {S1, S2}
    end.
