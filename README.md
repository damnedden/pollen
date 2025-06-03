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

You will find a full list of available commands with /join

🚀 MIT Server Team Challenge Instructions
-------

Instructions on how to run the challenge.

### Erlang project setup 

First of all let's boot the server with 

    $ docker compose up -d

There's no need to build anything manually, the release comes with the container through rebar3 release, after that we compile the client with

    $ erlc pollen_client.erl

And then we're ready to go.

To test the API effectively, it's recommended to open multiple terminals to simulate different TCP/IP clients communicating simultaneously.

### Accept multiple TCP/IP client connections at the same time

To test concurrent connections, use the preconfigured clients Alice and Bob:

    $ ./clients/alice   # in terminal 1
    $ ./clients/bob     # in terminal 2

### Handle rooms

The following commands test the various chat room features. Note: after typing a command, you'll be prompted for any necessary arguments.

    $ > /channel
    $ > /close
    $ > /list
    $ > /join
    $ > /leave
    $ > <message>

### Send private messages

To send a private message to another user:

    $ > /pmessage

### Handle private rooms

To test and handle a private room, we can use the following commands:

    $ > /pchannel
    $ > /invite

We can verify that some rooms are hidden from others by running the /list command from a different terminal.

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
