-module(rose_script).
-export([eval/1, test/0]).

-include("rose_opcodes.hrl").

eval(Script) ->
    eval(Script, {[], []}).

eval(<<H:8, T/binary>>, Stacks) ->
    {NewScript, NewStacks} = eval_code(H, T, Stacks),
    eval(NewScript, NewStacks);
eval(<<>>, Stacks) ->
    Stacks.

eval_code(Op, Script, {S, AS}) ->
    case Op of
        %% OP_0
        ?'0' ->
            { Script, {[0 | S], AS} };
        %% OP_PUSHDATA
        Op when Op >= 16#01 andalso Op =< 16#4b ->
            N = Op * 8,
            <<Data:N, NewScript/binary>> = Script,
            { NewScript, {[Data | S], AS} };
        %% OP_PUSHDATA1
        ?PUSHDATA1 ->
            <<L, Script1/binary>> = Script,
            N = L * 8,
            <<Data:N, NewScript/binary>> = Script1,
            { NewScript, {[Data | S], AS} };
        %% OP_PUSHDATA2
        ?PUSHDATA2 ->
            <<L:16, Script1/binary>> = Script,
            N = L * 8,
            <<Data:N, NewScript/binary>> = Script1,
            { NewScript, {[Data | S], AS} };
        %% OP_PUSHDATA4
        ?PUSHDATA4 ->
            <<L:32, Script1/binary>> = Script,
            N = L * 8,
            <<Data:N, NewScript/binary>> = Script1,
            { NewScript, {[Data | S], AS} };
        %% OP_1NEGATE
        ?'1NEGATE' ->
            { Script, {[-1 | S], AS} };
        %% OP_RESERVED(Invalid)
        %% OP_1..OP_16
        Op when Op >= ?'1' andalso Op =< ?'16' ->
            { Script, {[Op - 80 | S], AS} };
        %% OP_NOP
        ?NOP ->
            { Script, {S, AS} };
        %% OP_VER
        %% OP_IF
        %% OP_NOTIF
        %% OP_VERIF
        %% OP_VERNOTIF
        %% OP_ELSE
        %% OP_ENDIF
        %% OP_VERIFY
        %% OP_RETURN
        %% OP_TOALTSTACK
        ?TOALTSTACK ->
            [H|S1] = S,
            { Script, {S1, [H|AS]} };
        ?FROMALTSTACK ->
            [H|AS1] = AS,
            { Script, {[H|S], AS1} };
        ?'2DROP' ->
            [_, _|S1] = S,
            { Script, {S1, AS} };
        ?'2DUP' ->
            [H1, H2|_] = S,
            { Script, {[H1, H2|S], AS} };
        ?'3DUP' ->
            [H1, H2, H3|_] = S,
            { Script, {[H1, H2, H3|S], AS} };
        ?'2OVER' ->
            [_, _, H3, H4|_] = S,
            { Script, {[H3, H4|S], AS} };
        ?'2ROT' ->
            [H1, H2, H3, H4, H5, H6|S1] = S,
            { Script, {[H5, H6, H1, H2, H3, H4|S1], AS} };
        ?'2SWAP' ->
            [H1, H2, H3, H4|S1] = S,
            { Script, {[H3, H4, H1, H2|S1], AS} };
        ?IFDUP ->
            [H|_] = S,
            case H of
                0 ->
                    { Script, {S, AS} };
                _ ->
                    { Script, {[H|S], AS} }
            end;
        ?DEPTH ->
            Size = length(S),
            { Script, {[Size|S], AS} };
        ?DROP ->
            [_|S1] = S,
            { Script, {S1, AS} };
        ?DUP ->
            [H|_] = S,
            { Script, {[H|S], AS} };
        ?NIP ->
            [H, _|S1] = S,
            { Script, {[H|S1], AS} };
        ?OVER ->
            [_, H|_] = S,
            { Script, {[H|S], AS} };
        %% (xn ... x2 x1 x0 n - xn ... x2 x1 x0 xn)
        ?PICK ->
            [N|NewScript] = Script,
            ;
        %% (xn ... x2 x1 x0 n - ... x2 x1 x0 xn)
        ?ROLL ->
            [N|NewScript] = Script,
            ;
        %% (x1 x2 x3 -- x2 x3 x1)
        %%  x2 x1 x3  after first swap
        %%  x2 x3 x1  after second swap
        ?ROT ->
            [H1, H2, H3|S1] = S,
            { Script, {[H3, H1, H2|S1], AS} };
        %% (x1 x2 -- x2 x1)
        ?SWAP ->
            [H1, H2|S1] = S,
            { Script, {[H2, H1|S1], AS} };
        %% (x1 x2 -- x2 x1 x2)
        ?TUCK ->
            [H1, H2|S1] = S,
            { Script, {[H1, H2, H1|S1], AS} };


    end.

% %% push value
% opcode(16#00) ->                    '0';
% opcode(16#4c) ->            'PUSHDATA1';
% opcode(16#4d) ->            'PUSHDATA2';
% opcode(16#4e) ->            'PUSHDATA4';
% opcode(16#4f) ->              '1NEGATE';
% opcode(16#50) ->             'RESERVED';
% opcode(16#51) ->                    '1';
% opcode(16#52) ->                    '2';
% opcode(16#53) ->                    '3';
% opcode(16#54) ->                    '4';
% opcode(16#55) ->                    '5';
% opcode(16#56) ->                    '6';
% opcode(16#57) ->                    '7';
% opcode(16#58) ->                    '8';
% opcode(16#59) ->                    '9';
% opcode(16#5a) ->                   '10';
% opcode(16#5b) ->                   '11';
% opcode(16#5c) ->                   '12';
% opcode(16#5d) ->                   '13';
% opcode(16#5e) ->                   '14';
% opcode(16#5f) ->                   '15';
% opcode(16#60) ->                   '16';

% %% control
% opcode(16#61) ->                  'NOP';
% opcode(16#62) ->                  'VER';
% opcode(16#63) ->                   'IF';
% opcode(16#64) ->                'NOTIF';
% opcode(16#65) ->                'VERIF';
% opcode(16#66) ->             'VERNOTIF';
% opcode(16#67) ->                 'ELSE';
% opcode(16#68) ->                'ENDIF';
% opcode(16#69) ->               'VERIFY';
% opcode(16#6a) ->               'RETURN';

% %% stack ops
% opcode(16#6b) ->           'TOALTSTACK';
% opcode(16#6c) ->         'FROMALTSTACK';
% opcode(16#6d) ->                '2DROP';
% opcode(16#6e) ->                 '2DUP';
% opcode(16#6f) ->                 '3DUP';
% opcode(16#70) ->                '2OVER';
% opcode(16#71) ->                 '2ROT';
% opcode(16#72) ->                '2SWAP';
% opcode(16#73) ->                'IFDUP';
% opcode(16#74) ->                'DEPTH';
% opcode(16#75) ->                 'DROP';
% opcode(16#76) ->                  'DUP';
% opcode(16#77) ->                  'NIP';
% opcode(16#78) ->                 'OVER';
% opcode(16#79) ->                 'PICK';
% opcode(16#7a) ->                 'ROLL';
% opcode(16#7b) ->                  'ROT';
% opcode(16#7c) ->                 'SWAP';
% opcode(16#7d) ->                 'TUCK';

% %% splice ops
% opcode(16#7e) ->                  'CAT';
% opcode(16#7f) ->               'SUBSTR';
% opcode(16#80) ->                 'LEFT';
% opcode(16#81) ->                'RIGHT';
% opcode(16#82) ->                 'SIZE';

% %% bit logic
% opcode(16#83) ->               'INVERT';
% opcode(16#84) ->                  'AND';
% opcode(16#85) ->                   'OR';
% opcode(16#86) ->                  'XOR';
% opcode(16#87) ->                'EQUAL';
% opcode(16#88) ->          'EQUALVERIFY';
% opcode(16#89) ->            'RESERVED1';
% opcode(16#8a) ->            'RESERVED2';

% %% numeric
% opcode(16#8b) ->                 '1ADD';
% opcode(16#8c) ->                 '1SUB';
% opcode(16#8d) ->                 '2MUL';
% opcode(16#8e) ->                 '2DIV';
% opcode(16#8f) ->               'NEGATE';
% opcode(16#90) ->                  'ABS';
% opcode(16#91) ->                  'NOT';
% opcode(16#92) ->            '0NOTEQUAL';
% opcode(16#93) ->                  'ADD';
% opcode(16#94) ->                  'SUB';
% opcode(16#95) ->                  'MUL';
% opcode(16#96) ->                  'DIV';
% opcode(16#97) ->                  'MOD';
% opcode(16#98) ->               'LSHIFT';
% opcode(16#99) ->               'RSHIFT';
% opcode(16#9a) ->              'BOOLAND';
% opcode(16#9b) ->               'BOOLOR';
% opcode(16#9c) ->             'NUMEQUAL';
% opcode(16#9d) ->       'NUMEQUALVERIFY';
% opcode(16#9e) ->          'NUMNOTEQUAL';
% opcode(16#9f) ->             'LESSTHAN';
% opcode(16#a0) ->          'GREATERTHAN';
% opcode(16#a1) ->      'LESSTHANOREQUAL';
% opcode(16#a2) ->   'GREATERTHANOREQUAL';
% opcode(16#a3) ->                  'MIN';
% opcode(16#a4) ->                  'MAX';
% opcode(16#a5) ->               'WITHIN';

% %% crypto
% opcode(16#a6) ->            'RIPEMD160';
% opcode(16#a7) ->                 'SHA1';
% opcode(16#a8) ->               'SHA256';
% opcode(16#a9) ->              'HASH160';
% opcode(16#aa) ->              'HASH256';
% opcode(16#ab) ->        'CODESEPARATOR';
% opcode(16#ac) ->             'CHECKSIG';
% opcode(16#ad) ->       'CHECKSIGVERIFY';
% opcode(16#ae) ->        'CHECKMULTISIG';
% opcode(16#af) ->  'CHECKMULTISIGVERIFY';

% %% reserved opcodes
% opcode(16#b0) ->                 'NOP1';
% opcode(16#b1) ->  'CHECKLOCKTIMEVERIFY';
% opcode(16#b2) ->  'CHECKSEQUENCEVERIFY';
% opcode(16#b3) ->                 'NOP4';
% opcode(16#b4) ->                 'NOP5';
% opcode(16#b5) ->                 'NOP6';
% opcode(16#b6) ->                 'NOP7';
% opcode(16#b7) ->                 'NOP8';
% opcode(16#b8) ->                 'NOP9';
% opcode(16#b9) ->                'NOP10'.


test() ->
    Input = binary:encode_unsigned(16#3045022100b596a38e51dee771dfbb859b781f86a63b703ec5007e8fd5af50570b7da10e7b022033b9a3cc2351f83f95d84a7389fadb75448dc9ef48296f76e5c8e8938c1e4ce94103bd27657c38c0feab243da9c7e00ba2412408e1bb1d79367609f0a649f503c9aa),
    eval(Input).