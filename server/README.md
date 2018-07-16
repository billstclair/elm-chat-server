This directory is for uploading to the web server machine for the server-side of wws/elm-chat-server.

The server runs in `Node.js`.

Installing Node & Elm in Ubuntu (as root):

* `apt-get install npm`
* `apt-get install nodejs-legacy`
* `npm install -g elm`
* `npm install -g elm-websocket-server`

Installing the server code (user account, see [`package.json`](package.json)) for details):

One time:

* `cd .../server   # this directory`
* `npm install`

To build after a code change:

* `npm run build:server`

To start the server:

* `npm run start:server`

If your web server automatically upgrades to HTTPS, or you prefer to leave off the ":8081" from the Server URL, you'll need to proxy to get to the non-encrypted websocket server. Do this by installing Apache `mod_proxy_wstunnel`:

    $ sudo a2enmod proxy_wstunnel
    $ sudo service apache2 restart

Then add to either your Apache virtual host configuration or to an `.htaccess` file, the following:

    ProxyPass "/my-server"  "ws://localhost:8081/"
    
`/my-server` has to match the contents of `site/server.txt`, from which the client loads the server default.

If you're running the server on your local machine, you can aim your browser at:

    http://localhost:8081
    
to get a very simple test client that sends the strings you type over the wire and prints what it receives back.

If you want to run your server on a port other than 8081, you can set the `PORT` environment variable:

* `PORT=8800 npm run start:server`

There are a couple of scripts in the `bin` directory:

* `bin/m <foo>` runs `elm make` on `../src/<foo>.elm`, discarding the output.
* `bin/update-site` runs my [`rsyncit`](https://github.com/billstclair/wws-scripts#rsyncit) script on this directory, excluding the binary output directories. This uploads changes to the server specified by `.sshdir`.
