lmtp_poc
========

This repository is a proof of concept for the modifications made in `gen_smtp`
to implement the LMTP protocol
([github:abravalheri/gen_smtp@add-lmtp](https://github.com/abravalheri/gen_smtp/tree/add-lmtp)).
The implementation is very simplified and just stores mail files (`*.eml`) in
the file system.

`src/lmtp_server.erl` implements the callbacks necessary for
`gen_smtp_server_session`. It is important to notice that this behaviour
requires the implementation of some functions that are not exactly used for
LMTP: `handle_HELO` and `handle_EHLO`.

`handle_HELO` should always return an error, because an LMTP should not respond
to the `HELO` command. `handle_EHLO`, despite the name, will actually be the
function responsible for replying to the `LHLO` command.

The server can be configured in the following ways:

- the `LMTP_PORT` and `LMTP_DOMAIN` environment variables are used to set up
  the TCP socket.
- the `MAIL_DIR` environment variable defines a directory where the emails will
  be stored. If a `blocked.txt` file is defined inside `MAIL_DIR` with an email
  per line, the corresponding senders will be blocked by the LMTP server.


Testing
-------

Since the main objective of this repository is to test the proposed changes,
some simple tests were implemented.

To run the "unit"(-ish)/functional tests, the following command can be used:

    $ rebar3 eunit

To run system tests (using docker), the following script can be used:

    $ ./test/system.sh

The system tests are very simple and consist basically in running
[OpenSMTPD](https://www.opensmtpd.org/)
in a container that hands all the received emails to a second container
running the implemented service.

You can also see it in action and test the implementation manually.

1. First you need to run the docker containers:
    ```bash
    $ ./docker/build.sh
    $ ./docker/run.sh
    ```
2. Then "tap" inside the OpenSMTPD container:
    ```bash
    $ docker-compose -f docker/compose.yml exec smtp ash
    ```
3. and use sendmail localy:
    ```bash
    $ echo "Subject: hello" | sendmail -f sender@test postmaster@localhost
    ```

To see the received emails, "tap" into the LMTP server, and check the folders
for each recipient:
    ```bash
    $ docker-compose -f docker/compose.yml exec lmtp ash
    $ ls postmaster@localhost  # see received emails as .eml files
    $ cat postmaster@localhost/*.eml
    ```

For simplicity, OpenSMTPD and the LMTP server are configured to accept only
emails for `postmaster@localhost` and `operator@localhost`.
