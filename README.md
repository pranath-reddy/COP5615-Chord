# Chord Peer-to-Peer System Implementation and Simulation

Developed as part of coursework for COP5615 - Distributed Operating System Principles  
  
**Programming Language:** Erlang

## Description

The objective of the project is to implement a network join and routing algorithm referred to as Chord. We implement the Chord protocol and a simple object access service to prove its usefulness using Erlang and the Actor Model based on the specification of the Chord protocol given by Stoica et al. The bonus implementation adds a node failure model to the simulation.

* Chord is a protocol and algorithm for a peer-to-peer distributed hash table.
* A distributed hash table stores key-value pairs by assigning keys to different nodes. A node will store the values for all the keys for which it is responsible.
* The Chord protocol specifies how keys are assigned to nodes and how a node can discover the value of a given key by first locating the node responsible for that key.

## Execution

**Compile:** ```c(project3).```   
**Execute:** ```project3:main(N, R).```   
* *N - Network Size (Number of Nodes)*  
* *R - Number of Requests*  

## Reports 
  
[Main Report](https://github.com/pranath-reddy/COP5615-Chord/blob/main/Report.pdf)  
[Bonus Implementation](https://github.com/pranath-reddy/COP5615-Chord/blob/main/Report-bonus.pdf)
