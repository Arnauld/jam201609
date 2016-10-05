## Joe Amstrong - Message protocol vs. Function call API

> The users program call functions in a module. These are defined in an API (A).
> The library which implements the API works together with a messaging interface (B).
>
> The top-level interface (A) is defined in terms of typed function calls.
> The messaging interface (B) is defined in terms of message flows.
>
> Now (A) is usually well defined and should be stable over many years.
> (B) might change with time. This has the advantage that we can change B 
> as we learn more > while not damaging applications that use A.
> 
> There are some problems with this: Usually the interface (B) is not
> well specified and has to be inferred by reading the code. Also for efficiency 
> reasons a user program might like to bypass (A) and talk to the (B) interface 
> directly, the latter will cause problems if the (B) protocol is changed.
> 
> This is exactly how the erlang system is structured. A user program doing io
> will call io:read(Pid, X) (ie a function according to the (A) interface) the
> implementation of `io:read` will send a `{get_until,unicode,X,erl_scan,tokens,[1]}` 
> message across the (B) interface, and so on ...
> 
> The (A) interface is well defined and stable. The (B) interface is not
> well documented (read the code Luke).
> 
> The is not too bad. Usually only one person has to understand the (B)
> interface (ie the person who implements the library) - whereas large
> numbers of people have to understand the (A) interface.
> 
> Taking a problem and partitioning it into (A) and (B) interfaces is
> not easy. You can start with (A) and "invent" the necessary (B)
> interface (this is top-down design). or you can start with
> B and invent A (bottom-up design).
> 
> In the case where B is given we'd do bottom up design (for example B
> *is* TCP/IP or a given hardware interface like an SSD). For user
> convenience we'd like to start with A and derive B.
> 
> Sometimes we design A with a set of specific set of properties and B
> is given - so here we just have to write some glue in the middle (what I call a middle
> man) - an example of this might be AMQP (the Advanced Message Queue Protocol). 
> The designer of AMQP things "what would be a nice API to write messaging applications
> with?" - answer "reliable messaging with, security and notifications" - so (A) is
> given. (B) might be TCP/IP or UDP or whatever ...
> 
> For some projects I start by defining (A) then invent (B), or use a given (B). 
> others B is given and I figure out an (A) that seems appropriate.
> 
> "It depends ..." :-)
> 
> /Joe
> [Message protocol vs. Function call API](http://erlang.org/pipermail/erlang-questions/2009-October/046794.html)

## Quora - What is the difference between an API and a Protocol?

> API describes all the valid messages that one program can accept. It says nothing about 
> the proper ordering of these messages
>
> Protocols sit on top of APIs. A protocol describes the valid sequence of messages that 
> flow between the APIs of multiple parties to accomplish some higher-level task.
>
> [Quora - What is the difference between an API and a Protocol?](https://www.quora.com/What-is-the-difference-between-an-API-and-a-Protocol)
