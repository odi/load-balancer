## Load Balancer implementation in Haskell

This is a more or less intelligent implementation of a load balancer
written in Haskell. There are currently three REST entry points:

`/register` to register a new service

`/info`     to get info about the registered services

`/send`     to send an abitrary request to one of the services

Currently there is one function for selecting a service
implemented. It is a round-robin function.
