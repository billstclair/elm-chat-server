# elm-chat-server

An example of using [elm-websocket-framework](http://package.elm-lang.org/packages/billstclair/elm-websocket-framework/latest).

The client is implemented by [src/ChatClient.elm](src/ChatClient.elm). You can run it in `elm-reactor` by clicking that link. To start `elm-reactor`:

    cd .../elm-chat-server
    elm reactor
    
Then aim your web browser at [localhost:8000](http://localhost:8000).

There are some scripts in the [bin](bin/) directory:

1. "`m <class>`" compiles `src/<class>.elm`, throwing away output.

2. "`build-site`" compiles `src/ChatClient.elm` into `site/index.html`.

3. "`update-site`" runs `build-site`, then uses my [`rsyncit`](https://github.com/billstclair/wws-scripts#rsyncit) script to upload the `site` directory to the server specified by [`site/.sshdir`](site/.sshdir).

The client will attempt to load the file `server.txt` from its top-level directory. If it finds it, it will replace the "Server" field with the string found there. It should be a WebSocket URL, e.g. "`ws://localhost:8081`" or "`wss://xossbow.com/chat`". The default value for the "Server" is "`ws://localhost:8081`", a local server running on the default port.

See the [`server`](server/) directory's README for instructions on installing, building, and running the server.

There's a live client, pointing at a live server, at [xossbow.com/chat](https://xossbow.com/chat/).

There are notes on the code in the README file in the [src](src/) directory.
