
Agenda
======

- Welcome & setup
  - Trifork
  - Check abilities (functional coding? Erlang syntax?)
  - The plan: to setup up and play with a distributed system
    (chatroom-ish). Note, no-tamper promise
  - Setup - software (Erlang; editor) & net

- Chat demo - presenting the chatroom, demoing the client
  - The chatroom protocol: {join,self()} + normal messages
  - (Publish-subscriber pattern)
  - Chat note: about non-use of OTP
    To keep it simple and focus on the language and core model

- Bot reading: echo_bot
  - A bit of Erlang syntax

- Free Play
  - (Pair up if appropriate)
  - Suggestions for things to do:
    - A monster bot for fighting with, with HP/respawn and stuff
    - Chatroom/chat client: join with names, show who says what
    - Chatroom: listing participants
    - Read some of the code
    - Dice bot  (simple or "roll 2d8")
    - Fight simulator (combat rules implementation)
    - Simple math: Primality tester; Fibonacci; ...
    - Storage bot (remember/recall/forget)
    - Grep bot

Erlang essentials
-----------------
- Varibles vs. atoms
- Pattern matching
- Data are immutable
- Variables are single-assignment
- Strings are lists of charpoints (integers)
- Lightweight processes
- Message send will copy the data being sent


Abstract -- Hands-on Erlang
===========================

Want to learn about Erlang?  Join us for a late afternoon session
where your will write some actually Erlang code.  To have a place to
start we have made a small chat application
(https://github.com/eriksoe/ErlangPlayBase) which will form the basis
for this hands-on session.

Your should bring your own laptop with Erlang already installed (for
install instructions go here:
https://www.erlang-solutions.com/downloads/download-erlang-otp).  If
you are not able to bring a laptop, we will try to pair you up with
another participant.

We look forward to see you.