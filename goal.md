# Tilapia

A userspace TCP stack.

I will be mainly following along with this
[guide](https://www.saminiir.com/lets-code-tcp-ip-stack-1-ethernet-arp/)

> A tilapia is a kind of fish.

## Tap Device
We will be dealing with a TAP device, a simulated Layer 2 device.
When we create our device we are assigned a MAC address at random
by the operating system, but we can choose our own if we want to:
`ip link set address aa::bb::cc::dd::ee::ff Tilapia`

This means we will be sending and receiving Ethernet frames.

# Tilapia

The goal of this TCP stack are:
* Implement a subset of TCP successfully
* Show what is going on behind the scenes

We can toggle this behaviour on or off by sending a **SIGUSR1** signal.
We can also toggle disabling all outbound writes with a *SIGUSR2* signal.

To do this, just execute the following in a shell on the tilapia host:

``` kill -s SIGUSR1 $(pidof tilapia) ```
