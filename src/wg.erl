-module(wg).

-export([genkey/0]).
-export([genpsk/0]).
-export([pubkey/1]).
-export([add_peer/3]).
-export([remove_peer/2]).
-export([peers/1]).

genkey() ->
    {0, Key} = exec(<<"wg genkey">>), Key.

genpsk() ->
    {0, Key} = exec(<<"wg genpsk">>), Key.

pubkey(PrivKey) when is_binary(PrivKey) ->
    {0, PubKey} = exec(<<"echo ", PrivKey/binary, " | wg pubkey">>), PubKey.

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
