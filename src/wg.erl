-module(wg).

-export([genkey/0]).
-export([genpsk/0]).
-export([pubkey/1]).
-export([add_peer/2]).
-export([add_peer/3]).
-export([remove_peer/1]).
-export([remove_peer/2]).
-export([peers/0]).
-export([peers/1]).
-export([dump/0]).
-export([dump/1]).
-export([peer/1]).
-export([peer/2]).
-export([is_peer_connected/1]).
-export([is_peer_connected/2]).

genkey() ->
    {0, Key} = exec(<<"wg genkey">>), Key.

genpsk() ->
    {0, Key} = exec(<<"wg genpsk">>), Key.

pubkey(PrivKey) when is_binary(PrivKey) ->
    {0, PubKey} = exec(<<"echo ", PrivKey/binary, " | wg pubkey">>), PubKey.

add_peer(PubKey, AllowedIPs) ->
    add_peer(ifname(), PubKey, AllowedIPs).

add_peer(Ifname, PubKey, AllowedIPs) ->
    Cmd = iolist_to_binary([
        "sudo wg set ",
        Ifname,
        " peer ",
        PubKey,
        " allowed-ips ",
        string:join(AllowedIPs, ",")
    ]),
    case exec(Cmd) of
        {0, <<>>} -> ok;
        {_Code, Reason} ->
            {error, Reason}
    end.

remove_peer(PubKey) ->
    remove_peer(ifname(), PubKey).

remove_peer(Ifname, PubKey) ->
    Cmd = iolist_to_binary([
        "sudo wg set ",
        Ifname,
        " peer ",
        PubKey,
        " remove"
    ]),
    case exec(Cmd) of
        {0, <<>>} -> ok;
        {_Code, Reason} ->
            {error, Reason}
    end.

peers() ->
    peers(ifname()).

peers(Ifname) ->
    Cmd = iolist_to_binary([
        "sudo wg show ",
        Ifname,
        " peers"
    ]),
    case exec(Cmd) of
        {0, Data} ->
            {ok, binary:split(Data, <<"\n">>, [global, trim_all])};
        {_Code, Reason} ->
            {error, Reason}
    end.

dump() ->
    dump(ifname()).

dump(Ifname) ->
    Cmd = iolist_to_binary([
        "sudo wg show ",
        Ifname,
        " dump | tail -n +2"
    ]),
    {0, Data} = exec(Cmd),
    lists:foldl(
        fun(Line, Acc) ->
            [PubKey, _PreKey, _EP, _AI, LH, Rx, Tx, KA] = binary:split(Line, <<"\t">>, [global, trim_all]),
            Acc#{
                PubKey => #{
                    lh => binary_to_integer(LH), %% latest-handshake
                    rx => binary_to_integer(Rx), %% transfer-rx
                    tx => binary_to_integer(Tx), %% transfer-tx
                    ka => case KA of             %% persistent-keepalive
                        <<"off">> -> 0;
                        _ -> binary_to_integer(KA)
                    end
                }
            }
        end,
        #{},
        binary:split(Data, <<"\n">>, [global, trim_all])
    ).


peer(PubKey) ->
    peer(ifname(), PubKey).

peer(Ifname, PubKey) ->
    maps:get(PubKey, dump(Ifname), undefined).

is_peer_connected(PubKey) ->
    is_peer_connected(ifname(), PubKey).

is_peer_connected(Ifname, PubKey) ->
    Now = erlang:system_time(second),
    case maps:get(PubKey, dump(Ifname), undefined) of
        #{lh := LH} when Now - LH < 180 ->
            true;
        _ -> false
    end.

exec(Command) ->
    Port = open_port({spawn, Command}, [stream, in, stderr_to_stdout, eof, hide, exit_status, binary]),
    get_data(Port, <<>>, 5000).

get_data(Port, Acc, Timeout) ->
    receive
        {Port, {data, Bytes}} ->
            get_data(Port, <<Acc/binary, Bytes/binary>>, Timeout);
        {Port, eof} ->
            port_close(Port),
            ExitCode =
                receive
                    {Port, {exit_status, Code}} ->
                        Code
                end,
            {ExitCode, string:trim(Acc)}
    after Timeout ->
        {1, Acc}
    end.

ifname() ->
    case persistent_term:get({?MODULE, ifname}, undefined) of
        undefined ->
            persistent_term:put({?MODULE, ifname}, application:get_env(?MODULE, ifname, <<"wg0">>)),
            ifname();
        Ifname -> Ifname
    end.
