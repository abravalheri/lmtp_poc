% Just some sanity checks to make sure nothing is too wrong,
% based on `gen_smtp_server_session' tests.
-module(lmtp_server_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MAIL_DIR, "test/fixtures/maildir").
-define(setup(F),
        ?_test(begin
                   Arg = setup(),
                   try
                       F(Arg)
                   after
                       teardown(Arg)
                   end
               end)).
-define(assertBanner(CSock, Pattern),
        begin
            smtp_socket:active_once(CSock),
            ?assertMatch(Pattern, receive_packet(CSock))
        end).
-define(assertReply(CSock, Send, Pattern),
        begin
            smtp_socket:send(CSock, Send),
            ?assertMatch(Pattern, receive_packet(CSock))
        end).
-define(assertSuccessful(CSock, Send), ?assertReply(CSock, Send, "250 " ++ _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
lmtp_server_test_() ->
    {"The server should be able to receive an email", ?setup(fun email_with_multiple_RCPT/1)}.

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
setup() ->
    true = os:putenv("MAIL_DIR", ?MAIL_DIR),
    ?debugVal(os:getenv("MAIL_DIR")),
    {ok, Pid} = application:ensure_all_started(gen_smtp),
    lmtp_server:start([{domain, "localhost"}, {port, 9876}]),
    {ok, CSock} = smtp_socket:connect(tcp, "localhost", 9876),
    {CSock, Pid}.

teardown({CSock, _Pid}) ->
    lmtp_server:stop(),
    smtp_socket:close(CSock),
    timer:sleep(10),
    ?debugVal("END").

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
email_with_multiple_RCPT({CSock, _Pid}) ->
    ?assertBanner(CSock, "220 localhost" ++ _),
    ?assertReply(CSock, "LHLO somehost.com\r\n", "250-localhost\r\n"),
    ?assertEqual(handshake(CSock), ok),
    ?assertSuccessful(CSock, "MAIL FROM:<user@otherhost>\r\n"),
    ?assertSuccessful(CSock, "RCPT TO:<test1@localhost>\r\n"),
    ?assertSuccessful(CSock, "RCPT TO:<test2@localhost>\r\n"),
    ?assertReply(CSock, "RCPT TO:<test3@somehost.com>\r\n", "550 No such recipient" ++ _),
    ?assertReply(CSock, "DATA\r\n", "354 " ++ _),
    smtp_socket:send(CSock,
                     ["Subject: tls message\r\n",
                      "To: <test1@localhost>\r\n",
                      "From: <user@somehost.com>\r\n",
                      "\r\n",
                      "message body",
                      "\r\n.\r\n"]),
    assert_n_deliveries(CSock, 2),
    ?assertReply(CSock, "QUIT\r\n", "221 " ++ _),
    %% Test check -- uncomment to make sure the test works
    % X = rand:uniform(),
    % ?assertEqual(X, 2),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
receive_packet(CSock) ->
    receive
        {tcp, CSock, Packet} ->
            smtp_socket:active_once(CSock),
            Packet
    end.

handshake(CSock) ->
    receive
        {tcp, CSock, "250-" ++ _} ->
            smtp_socket:active_once(CSock),
            handshake(CSock);
        {tcp, CSock, "250 PIPELINING" ++ _} ->
            smtp_socket:active_once(CSock),
            ok;
        {tcp, CSock, Data} ->
            smtp_socket:active_once(CSock),
            {error, ["received: ", Data]}
    end.

assert_n_deliveries(_, 0) ->
    ok;
assert_n_deliveries(CSock, N) ->
    ?assertMatch("250 " ++ _,
                 receive
                     {tcp, CSock, Packet} ->
                         smtp_socket:active_once(CSock),
                         Packet
                 end),
    assert_n_deliveries(CSock, N - 1).
