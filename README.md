pollen 🌻
============

A cozy, lightweight application to run a simple CLI chat server capable of hosting multiple channels, handle private messages and more!

In this repository there's both a client application to connect to pollen servers and the server application, all written in Erlang/OTP25.

Features included:
- A global channel for everyone to meet.
- Public/private chat channels.
- Invite-only conversation.
- Private messaging between users.
- Clean CLI interface with colors.
- Fully concurrent and lightweight by design.

How to Run (in Docker)
-------

You will have to spin up the server container, this will automatically create a new release with the rebar3 release command.

    $ docker compose up -d

Your pollen server is now live and ready to accept connections at

    $ 0.0.0.0:4000

Usage (in Docker)
-------

To test the application in an isolated environment, you can access the server container's shell. It's recommended to open multiple terminal sessions to simulate interactions between different clients.

    $ docker exec -it pollen-server /bin/bash

Once inside, compile the client module and start an Erlang shell to connect:

    $ erlc pollen_client.erl
    $ erl -noshell -sname alice -eval "p:main([\"localhost\",\"4000\",\"Alice\"])"

For convenience, you’ll find three pre-configured clients inside the **/clients/** directory. After compilation, use them to test the various features of the server.

You will find a full list of available commands with /help

Hosting
-------

By default your pollen server will be hosted on port **4000** but you can configure this and other options under **/includes/env.hrl**. Make sure to change the port inside the docker-compose.yaml aswell

Your clients will be able to:

- Use /join to enter or create a channel
- Use /channel to create a public channel
- Many more commands available with /help

License
-------
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
